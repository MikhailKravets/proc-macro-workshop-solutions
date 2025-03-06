use std::collections::HashMap;

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{spanned::Spanned, Data, DeriveInput, Error, Ident, Lit, Meta, PathArguments, Type, TypePath};

type ItemField<'a> = (&'a Ident, &'a syn::TypePath, &'a Vec<syn::Attribute>, bool);

/// Retrieve struct data from &Data reference
fn retrieve_data(ast: &Data) -> syn::Result<&syn::DataStruct> {
    match ast {
        Data::Struct(d) => Ok(d),
        _ => Err(syn::Error::new(Span::call_site(), "Derivee object should be struct"))
    }
}

/// Generates TokenStream containing error message
/// 
/// # Attributes
/// * `t` - spanned object for which prints the error
/// * `msg` - error message
fn span_error<T: Spanned, M: AsRef<str>>(t: T, msg: M) -> TokenStream {
    syn::Error::new(t.span(), msg.as_ref()).into_compile_error().into()
}

/// Having Option<T> return `proc_macro2::TokenStream`
/// that corresponds to the inner type `T`.
/// 
/// If Option is not present, return the original `ty`.
fn extract_from(ty: &TypePath, ident: &str) -> TokenStream2 {
    for v in ty.path.segments.iter() {
        if v.ident == ident {
            match &v.arguments {
                PathArguments::AngleBracketed(ab) => {
                    let args = &ab.args;
                    return quote! {#args};
                }
                _ => ()
            }
        }
    };

    quote! {#ty}
}

/// Extract MetaList from the attribute which is helpful
/// during compile error generation.
fn extract_attribute_list(attr: &syn::Attribute) -> Option<&syn::MetaList> {
    match &attr.meta {
        Meta::List(list) => Some(list),
        _ => None
    }
}

/// Extract an attribute with a specific name. It extracts the information
/// from `key = value` attributes
fn extract_attribute<'a>(attrs: &'a [syn::Attribute], name: &'a str) -> syn::Result<Option<(HashMap::<String, String>, Option<&'a syn::MetaList>)>> {
    let attr = match attrs.iter().find(|v| v.path().is_ident(name)) {
        Some(attr) => attr,
        None => return Ok(None)
    };

    let mut map = HashMap::new();
    attr.parse_nested_meta(|meta| {
        let ident = match meta.path.get_ident() {
            Some(ident) => ident,
            None => return Err(meta.error("Identifier should not be empty."))
        };
        let value = meta.value()?.parse()?;

        if let Lit::Str(v) = value {
            map.insert(ident.to_string(), v.value());    
        } else {
            return Err(meta.error(format!("`{}` property should accept a value of type  &'static str.", ident)));
        }

        Ok(())
    })?;

    Ok(Some((map, extract_attribute_list(attr))))
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(input).unwrap();
    // println!("{:#?}", ast);
    let name = ast.ident;
    let builder_name = Ident::new(&format!("{}Builder", &name), name.span());
    let data = match retrieve_data(&ast.data) {
        Ok(d) => d,
        Err(e) => {
            return e.to_compile_error().into();
        }
    };

    let mut fields: Vec<ItemField> = Vec::with_capacity(data.fields.len());
    for field in data.fields.iter() {
        let ident = field.ident.as_ref().unwrap();
        let ty = match &field.ty {
            Type::Path(ty) => ty,
            _ => return span_error(ident, "Field should have a well-defined type")
        };
        let attrs = &field.attrs;
        let is_optional = ty.path.segments.iter().find(|v| v.ident == "Option").is_some();
        fields.push((ident, ty, attrs, is_optional));
    }

    let builder_fields: Vec<TokenStream2> = fields.iter().map(|(name, ty, _, is_optional)| {
        if *is_optional {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
        
    }).collect();
    
    // let default_fields: Vec<TokenStream2> = fields.iter().map(|(name, ty, _, _)| {
    //     let mut token_stream = TokenStream2::new();
    //     let last = ty.path.segments.len() - 1;
    //     for (i, v) in ty.path.segments.iter().enumerate() {
    //         v.ident.to_tokens(&mut token_stream);
    //         if i < last {
    //             PathSep::default().to_tokens(&mut token_stream);
    //         }
    //     }
        
    //     quote! {#name: #token_stream::default()}
    // }).collect();

    let empty_fields: Vec<TokenStream2> = fields.iter().map(|(name, _, _, _)| {
        quote! {#name: std::option::Option::None}
    }).collect();
    

    let setters: Vec<TokenStream2> = fields.iter().map(|(name, ty, attrs, _)| {
        let attr_map = match extract_attribute(attrs, "builder") {
            Ok(map) => map,
            Err(e) => {
                return e.into_compile_error();
            }
        };
        match attr_map {
            Some((map, list)) => {
                let inner = extract_from(ty, "Vec");
                match map.get("each") {
                    Some(v) => {
                        let ident = Ident::new(v, Span::call_site());
                        quote! {
                            fn #ident(&mut self, value: #inner) -> &mut Self {
                                match self.#name {
                                    Some(ref mut v) => v.push(value),
                                    None => self.#name = Some(vec![value])
                                };
                                self
                            }
                        }
                    },
                    None => return Error::new_spanned(list, "expected `builder(each = \"...\")`").into_compile_error()
                }
            },
            None => {
                let inner = extract_from(ty, "Option");
                quote! {
                    fn #name(&mut self, value: #inner) -> &mut Self {
                        self.#name = std::option::Option::Some(value);
                        self
                    }
                }
            }
        }
    }).collect();

    // println!("{:#?}", setters);

    let build_fields: Vec<TokenStream2> = fields.iter().map(|(name, _, _, is_optional)| {
        if *is_optional {
            quote! { #name: std::mem::take(&mut self.#name) }
        } else {
            quote! { #name: std::mem::take(&mut self.#name).unwrap_or_default() }
        }
    }).collect();

    let out = quote! {
        struct #builder_name {
            #(#builder_fields),*
        }

        impl #builder_name {
            #(#setters)*

            fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                std::result::Result::Ok(
                    #name {
                        #(#build_fields),*
                    }
                )
            }
        }

        impl #name {
            fn builder() -> #builder_name {
                #builder_name {
                    #(#empty_fields),*
                }
            }
        }
    };

    // println!("{:#?}", out);

    out.into()
}