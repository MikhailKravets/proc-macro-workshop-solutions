use std::collections::HashMap;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, parse_quote, spanned::Spanned, DeriveInput};

#[derive(Debug)]
enum AttrKind {
    Bound((String, syn::WherePredicate)),
    Format(String),

    /// Special case to avoid using `Option<T>`
    None,
}

/// Find attribute with a `name` and extract
/// the value with a `key`
fn extract_attribute_value<'a>(attrs: &'a [syn::Attribute], key: &'a str) -> syn::Result<AttrKind> {
    let attr = match attrs.iter().find(|v| v.path().is_ident(key)) {
        Some(v) => v,
        None => return Ok(AttrKind::None),
    };

    let value = &attr.meta;

    match value {
        syn::Meta::NameValue(v) => {
            let lit = match &v.value {
                syn::Expr::Lit(s) => s,
                _ => return Err(syn::Error::new(value.span(), "Unsupported attribute.")),
            };

            match &lit.lit {
                syn::Lit::Str(s) => Ok(AttrKind::Format(s.value())),
                _ => Err(syn::Error::new(
                    lit.span(),
                    "Unsupported attribute type. Only string is allowed.",
                )),
            }
        }
        syn::Meta::List(v) => {
            let mut res = AttrKind::None;
            v.parse_nested_meta(|meta| {
                if meta.path.is_ident("bound") {
                    let value: syn::LitStr = meta.value()?.parse()?;
                    let bound = value.value();

                    if let Some((lhs, _)) = bound.split_once("::") {
                        res = AttrKind::Bound((lhs.into(), syn::parse_str(&bound)?));
                    }
                }

                Ok(())
            })?;

            Ok(res)
        }
        _ => Err(syn::Error::new(
            value.span(),
            "Unsupported attribute type. Use #[debug = string format].",
        )),
    }
}

/// Add `std::fmt::Debug` type bounds for generics.
///
/// # Arguments
///
/// * `generics` - generic tokens to add bounds for
/// * `special` - a hash map of tokens which should be ignored
///     and corresponding `syn::Path` objects to which
///     `std::fmt::Debug` should be added   
fn add_type_bounds(
    mut generics: syn::Generics,
    special: HashMap<String, syn::WherePredicate>,
) -> syn::Generics {
    for v in generics.params.iter_mut() {
        match v {
            syn::GenericParam::Type(ref mut t) => {
                let s = t.ident.to_string();
                if !special.contains_key(&s) {
                    t.bounds.push(parse_quote! {std::fmt::Debug});
                }
            }
            _ => continue,
        }
    }

    let w = generics.make_where_clause();
    for v in special.values() {
        w.predicates.push(parse_quote! { #v });
    }

    generics
}

fn phantom_generics(fields: &syn::Fields) -> HashMap<String, syn::WherePredicate> {
    let mut map = HashMap::new();

    for field in fields.iter() {
        if let syn::Type::Path(ty) = &field.ty {
            for seg in ty.path.segments.iter() {
                if seg.ident != "PhantomData" {
                    continue;
                }

                match seg.arguments {
                    syn::PathArguments::AngleBracketed(ref args) => {
                        for gen in args.args.iter() {
                            match gen {
                                syn::GenericArgument::Type(syn::Type::Path(gen_ty)) => {
                                    if let Some(ident) = gen_ty.path.get_ident() {
                                        let p = &ty.path;
                                        map.insert(
                                            ident.to_string(),
                                            parse_quote! { #p: std::fmt::Debug },
                                        );
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    map
}

fn associated_bounds(fields: &syn::Fields) -> HashMap<String, syn::WherePredicate> {
    let mut map = HashMap::new();

    for field in fields.iter() {
        if let syn::Type::Path(ty) = &field.ty {
            for ty in ty.path.segments.iter() {
                match ty.arguments {
                    syn::PathArguments::AngleBracketed(ref args) => {
                        for gen in args.args.iter() {
                            match gen {
                                syn::GenericArgument::Type(syn::Type::Path(gen_ty)) => {
                                    if let Some(seg) = gen_ty.path.segments.first() {
                                        let p = &gen_ty.path;
                                        map.insert(
                                            seg.ident.to_string(),
                                            parse_quote! { #p: std::fmt::Debug },
                                        );
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    map
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    // println!("{:#?}", ast);
    // return quote! {}.into();

    let data_struct = match ast.data {
        syn::Data::Struct(s) => s,
        _ => {
            return syn::Error::new(
                Span::call_site(),
                "CustomDebug macro is available only for structs.",
            )
            .into_compile_error()
            .into();
        }
    };

    let struct_ident = &ast.ident;
    let struct_ident_str = struct_ident.to_string();

    let mut special = associated_bounds(&data_struct.fields);
    let phantom = phantom_generics(&data_struct.fields); // except has higher priority
    special.extend(phantom);

    let struct_attr = match extract_attribute_value(&ast.attrs, "debug") {
        Ok(value) => value,
        Err(e) => return e.into_compile_error().into(),
    };

    if let AttrKind::Bound((lhs, rhs)) = struct_attr {
        special.insert(lhs, parse_quote!(#rhs));
    }

    let fields: Vec<proc_macro2::TokenStream> = data_struct
        .fields
        .iter()
        .map(|field| {
            // TODO: how does it work with tuple structs?
            let ident = field.ident.as_ref().unwrap();
            let name = ident.to_string();
            let value = match extract_attribute_value(&field.attrs, "debug") {
                Ok(value) => value,
                Err(e) => return e.into_compile_error(),
            };

            match value {
                AttrKind::Format(v) => quote! {.field(#name, &format_args!(#v, &self.#ident))},
                AttrKind::Bound((lhs, rhs)) => {
                    special.insert(lhs, rhs);
                    quote! {.field(#name, &self.#ident)}
                }
                AttrKind::None => quote! {.field(#name, &self.#ident)},
            }
        })
        .collect();

    let generics = add_type_bounds(ast.generics, special);

    // println!("{:#?}", generics);
    // return quote! {}.into();
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    quote! {
        impl #impl_generics std::fmt::Debug for #struct_ident #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#struct_ident_str)
                #(#fields)*
                .finish()
            }
        }
    }
    .into()
}
