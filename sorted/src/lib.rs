use std::iter::Peekable;

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use syn::{spanned::Spanned, visit_mut::VisitMut};

enum PatternObject<'a> {
    Ident(&'a syn::Ident),
    Path(&'a syn::Path),
}

impl<'a> PatternObject<'a> {
    fn path_to_string(p: &syn::Path) -> String {
        let mut s = vec![];
        for v in p.segments.iter() {
            s.push(v.ident.to_string());
        }

        s.join("::")
    }

    fn to_string(&self) -> String {
        match self {
            Self::Ident(i) => i.to_string(),
            Self::Path(p) => Self::path_to_string(p),
        }
    }

    fn error(&self, msg: &str) -> syn::Error {
        match self {
            Self::Ident(i) => syn::Error::new(i.span(), msg),
            Self::Path(p) => syn::Error::new_spanned(p, msg),
        }
    }
}

fn check_ordering<'a>(variants: Vec<PatternObject>) -> syn::Result<()> {
    let original: Vec<(String, &PatternObject)> =
        variants.iter().map(|v| (v.to_string(), v)).collect();
    let mut sorted = original.clone();
    sorted.sort_by(|a, b| a.0.cmp(&b.0));

    for (orig, s) in original.into_iter().zip(sorted) {
        if orig.0 != s.0 {
            return Err(s.1.error(&format!("{} should sort before {}", s.0, orig.0)));
        }
    }

    Ok(())
}

fn verify_arm_type<'a>(it: impl Iterator<Item = &'a syn::Arm>) -> syn::Result<()> {
    for v in it {
        match v.pat {
            syn::Pat::TupleStruct(_) => continue,
            syn::Pat::Path(_) => continue,
            syn::Pat::Struct(_) => continue,
            _ => {
                return Err(syn::Error::new(
                    v.to_token_stream().span(),
                    "unsupported by #[sorted]",
                ))
            }
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

        if let Err(e) = verify_arm_type(i.arms.iter()) {
            self.errors.push(e);
            return;
        }

        if let Err(e) = check_ordering(
            i.arms
                .iter()
                .filter_map(|v| match &v.pat {
                    syn::Pat::TupleStruct(v) => Some(PatternObject::Path(&v.path)),
                    syn::Pat::Path(v) => Some(PatternObject::Path(&v.path)),
                    syn::Pat::Struct(v) => Some(PatternObject::Path(&v.path)),
                    _ => {
                        self.errors.push(syn::Error::new(
                            v.to_token_stream().span(),
                            "unsupported by #[sorted]",
                        ));
                        None
                    }
                })
                .collect(),
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

    match check_ordering(
        enum_object
            .variants
            .iter()
            .map(|v| PatternObject::Ident(&v.ident))
            .collect(),
    ) {
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
