use std::iter::Peekable;

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use syn::visit_mut::VisitMut;

fn check_ordering<'a>(variants: Peekable<impl Iterator<Item = &'a syn::Ident>>) -> syn::Result<()> {
    let original: Vec<&syn::Ident> = variants.collect();
    let mut sorted: Vec<&syn::Ident> = original.clone();
    sorted.sort();

    for (orig, s) in original.into_iter().zip(sorted) {
        if orig != s {
            return Err(syn::Error::new(
                s.span(),
                format!("{} should sort before {}", s, orig),
            ));
        }
    }

    Ok(())
}

struct Matcher {
    errors: Vec<syn::Error>,
}

impl Matcher {
    fn new() -> Self {
        Self {
            errors: Vec::default(),
        }
    }

    fn errors_to_token_stream(self) -> TokenStream2 {
        let mut res = TokenStream2::new();
        for e in self.errors {
            res.extend(e.to_compile_error());
        }

        res
    }
}

impl VisitMut for Matcher {
    fn visit_expr_match_mut(&mut self, i: &mut syn::ExprMatch) {
        let has_sorted = i
            .attrs
            .iter()
            .find(|v| v.path().is_ident("sorted"))
            .is_some();

        if !has_sorted {
            return;
        }

        // Remove #[sorted]
        i.attrs = i
            .attrs
            .iter()
            .filter(|v| !v.path().is_ident("sorted"))
            .cloned()
            .collect();

        if let Err(e) = check_ordering(
            i.arms
                .iter()
                .filter_map(|v| match &v.pat {
                    syn::Pat::TupleStruct(v) => Some(v.path.get_ident().unwrap()),
                    _ => None,
                })
                .peekable(),
        ) {
            self.errors.push(e);
        }
    }
}

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let item: syn::Item = syn::parse(input).unwrap();
    let enum_object = if let syn::Item::Enum(item) = item {
        item
    } else {
        return syn::Error::new(Span::call_site(), "expected enum or match expression")
            .to_compile_error()
            .into();
    };

    let mut res = TokenStream2::new();
    match check_ordering(enum_object.variants.iter().map(|v| &v.ident).peekable()) {
        Ok(_) => res.extend(enum_object.to_token_stream()),
        Err(e) => {
            res.extend(e.to_compile_error());
            res.extend(enum_object.to_token_stream());
        }
    };

    quote! { #res }.into()
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let item: syn::Item = syn::parse(input).unwrap();
    let mut fun = if let syn::Item::Fn(fun) = item {
        fun
    } else {
        return syn::Error::new(Span::call_site(), "expected fn")
            .to_compile_error()
            .into();
    };
    let mut matcher = Matcher::new();
    matcher.visit_item_fn_mut(&mut fun);
    println!("{:#?}", matcher.errors);
    let mut res = matcher.errors_to_token_stream();
    res.extend(fun.to_token_stream());

    quote! { #res }.into()
}
