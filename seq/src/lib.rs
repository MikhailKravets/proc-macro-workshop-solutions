use std::{collections::VecDeque, iter::Peekable, ops::Range, str::FromStr};

use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Group, Literal, TokenStream as TokenStream2, TokenTree};
use quote::{quote, ToTokens};
use syn::{braced, parse_macro_input, spanned::Spanned, Token};

fn lit_to_num<T>(lit: syn::LitInt) -> syn::Result<T>
where
    T: FromStr,
    T::Err: std::fmt::Display,
{
    lit.base10_parse()
}

/// Helper struct to use during round robin modification.
///
/// # Attributes
/// - `0` - how many items to pop from the back of the robin
/// - `syn::Ident` - object to insert to the back of the robin
#[derive(Debug)]
struct Reduce(u8, syn::Ident);

#[derive(Debug)]
struct RoundRobin {
    inner: VecDeque<TokenTree>,
}

impl RoundRobin {
    fn new() -> Self {
        Self {
            inner: VecDeque::default(),
        }
    }

    fn len(&self) -> usize {
        self.inner.len()
    }

    fn push(&mut self, value: TokenTree) {
        self.inner.push_back(value);
    }

    /// Substitutes the last value in round robin
    /// with the `value`
    fn set_last(&mut self, value: TokenTree) {
        let n = self.len() - 1;
        self.inner[n] = value;
    }

    /// A helper function that modifies round robin.
    ///
    /// # Arguments
    /// - `rr` - round robin to modify
    /// - `value` - reduce value to use for modification
    fn modify_robin(&mut self, value: Reduce) {
        for _ in 0..value.0 {
            self.inner.pop_back();
        }
        self.inner.push_back(TokenTree::Ident(value.1));
    }

    fn merge_single(&mut self, subs: usize) -> bool {
        if self.inner.len() == 0 {
            return false;
        }

        let reduce = match self.inner[self.inner.len() - 1] {
            TokenTree::Ident(ref n) => {
                if n == "N" {
                    true
                } else {
                    false
                }
            }
            _ => false,
        };

        if reduce {
            self.inner.pop_back();
            self.inner
                .push_back(TokenTree::Literal(Literal::usize_unsuffixed(subs)));
        }

        reduce
    }

    fn merge_prefix(&mut self, subs: usize) -> bool {
        let size = self.inner.len();
        if size < 3 {
            return false;
        }

        let reduce = match (
            &self.inner[size - 3],
            &self.inner[size - 2],
            &self.inner[size - 1],
        ) {
            (TokenTree::Ident(prefix), TokenTree::Punct(sep), TokenTree::Ident(n)) => {
                if sep.as_char() == '~' {
                    let new_name = format!("{}{}", prefix, subs);
                    let mut token = TokenStream2::new();
                    token.extend(prefix.to_token_stream());
                    token.extend(sep.to_token_stream());
                    token.extend(n.to_token_stream());

                    Some(Reduce(3, syn::Ident::new(&new_name, token.span())))
                } else {
                    None
                }
            }
            _ => None,
        };

        let return_value = reduce.is_some();

        if let Some(v) = reduce {
            self.modify_robin(v);
        }

        return_value
    }

    fn merge_full(&mut self, subs: usize) -> bool {
        let size = self.inner.len();
        if size < 5 {
            return false;
        }

        let reduce = match (
            &self.inner[size - 5],
            &self.inner[size - 4],
            &self.inner[size - 3],
            &self.inner[size - 2],
            &self.inner[size - 1],
        ) {
            (
                TokenTree::Ident(prefix),
                TokenTree::Punct(sep1),
                TokenTree::Ident(n),
                TokenTree::Punct(sep2),
                TokenTree::Ident(suffix),
            ) => {
                if sep1.as_char() == '~' && sep2.as_char() == '~' {
                    let new_name = format!("{}{}{}", prefix, subs, suffix);
                    let mut token = TokenStream2::new();
                    token.extend(prefix.to_token_stream());
                    token.extend(sep1.to_token_stream());
                    token.extend(n.to_token_stream());
                    token.extend(sep2.to_token_stream());
                    token.extend(suffix.to_token_stream());

                    Some(Reduce(5, syn::Ident::new(&new_name, token.span())))
                } else {
                    None
                }
            }
            (
                TokenTree::Ident(prefix),
                TokenTree::Punct(sep1),
                TokenTree::Ident(n),
                TokenTree::Punct(sep2),
                TokenTree::Literal(suffix),
            ) => {
                if sep1.as_char() == '~' && sep2.as_char() == '~' {
                    let new_name = format!("{}{}{}", prefix, n, suffix);
                    let mut token = TokenStream2::new();
                    token.extend(prefix.to_token_stream());
                    token.extend(sep1.to_token_stream());
                    token.extend(n.to_token_stream());
                    token.extend(sep2.to_token_stream());
                    token.extend(suffix.to_token_stream());

                    Some(Reduce(5, syn::Ident::new(&new_name, token.span())))
                } else {
                    None
                }
            }
            _ => None,
        };

        let return_value = reduce.is_some();
        if let Some(v) = reduce {
            self.modify_robin(v);
        }

        return_value
    }

    /// Reduce the round robin. The function is greedy and runs
    /// reduction one by one with the following priority:
    ///
    /// 1. _Full reduction_. Try to reduce full form of the regex, for example, an identifier `prefix~N~suffix`  
    ///    will be merged to `prefixNsuffix`.
    /// 2. _Prefix reduction_. Try to reduce prefix form of the regex, for example, an identifier
    ///    `prefix~N` is reduced to `prefixN`.
    /// 3. _Single reduction_. Try to reduce a single identifier of `N`.
    ///
    /// # Arguments
    /// - `rr` - round robin to reduce
    /// - `subs` - integer value that substitutes `N`
    fn reduce_robin(&mut self, subs: usize) {
        let _ = self.merge_full(subs) || self.merge_prefix(subs) || self.merge_single(subs);
    }

    /// Try to catch if previous to the last token is `~` and
    /// runs reduction. For example, if round robin contains
    /// `~token`, the reduction will be run, as well as for `~N`
    fn catch_and_reduce(&mut self, subs: usize) {
        if let Some(last) = self.inner.iter().take(self.len().saturating_sub(1)).last() {
            match last {
                TokenTree::Punct(p) => {
                    if p.as_char() == '~' {
                        self.reduce_robin(subs);
                    }
                }
                _ => (),
            }
        }
    }

    fn to_token_stream(self) -> TokenStream2 {
        let mut res = TokenStream2::new();
        for v in self.inner {
            res.extend(v.to_token_stream());
        }

        res
    }
}

#[derive(Debug)]
struct Loop {
    // ident: syn::Ident,
    // token_in: Token![in],
    // num_from: usize,
    // token_range: Token![..],
    // num_to: usize,
    range: Range<usize>,
    content: TokenStream2,
}

impl syn::parse::Parse for Loop {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let _ident = input.parse::<syn::Ident>()?;
        let _token_in = input.parse::<Token![in]>()?;

        let num_from: syn::LitInt = input.parse()?;
        let num_from: usize = num_from.base10_parse()?;
        let _token_range = input.parse::<Token![..]>()?;

        let lookahead = input.lookahead1();
        let range = if lookahead.peek(Token![=]) {
            let _ = input.parse::<Token![=]>()?;
            let num_to: usize = lit_to_num(input.parse()?)?;

            num_from..(num_to + 1)
        } else {
            let num_to: usize = lit_to_num(input.parse()?)?;

            num_from..num_to
        };

        let content;
        let _ = braced!(content in input);
        let content = content.parse()?;

        Ok(Loop { range, content }.expand())
    }
}

impl Loop {
    fn replace(i: usize, stream: TokenStream2) -> TokenStream2 {
        let mut iter = stream.into_iter().peekable();
        let mut rr = RoundRobin::new();

        while let Some(token) = iter.next() {
            rr.push(token.clone());
            match token {
                TokenTree::Group(n) => {
                    let mut group = Group::new(n.delimiter(), Self::replace(i, n.stream()));
                    group.set_span(n.span());
                    rr.set_last(TokenTree::Group(group));
                }
                TokenTree::Ident(n) => {
                    if n == "N" {
                        if let Some(lookahead) = iter.peek() {
                            match lookahead {
                                TokenTree::Punct(p) => {
                                    if p.as_char() != '~' {
                                        rr.reduce_robin(i);
                                    }
                                }
                                _ => rr.reduce_robin(i),
                            }
                        } else {
                            rr.reduce_robin(i)
                        }
                    } else {
                        rr.catch_and_reduce(i);
                    }
                }
                TokenTree::Literal(_skip) => rr.catch_and_reduce(i),
                TokenTree::Punct(_skip) => (),
            }
        }

        rr.to_token_stream()
    }

    fn multiply(stream: TokenStream2, range: Range<usize>) -> TokenStream2 {
        let mut res = TokenStream2::new();
        for i in range {
            res.extend(Self::replace(i, stream.clone()));
        }
        res
    }

    fn expand_iter(
        mut iter: Peekable<impl Iterator<Item = TokenTree>>,
        range: Range<usize>,
    ) -> (TokenStream2, bool) {
        let mut res = TokenStream2::new();
        let mut start_expansion = false;
        let mut is_found = false;

        while let Some(token) = iter.next() {
            match token {
                TokenTree::Ident(t) => {
                    res.extend(t.to_token_stream());
                    start_expansion = false;
                }
                TokenTree::Literal(t) => {
                    res.extend(t.to_token_stream());
                    start_expansion = false;
                }
                TokenTree::Group(t) => {
                    let d = t.delimiter();
                    if start_expansion {
                        if let (Some(TokenTree::Punct(p)), Delimiter::Parenthesis) =
                            (iter.peek(), d)
                        {
                            if p.as_char() == '*' {
                                is_found = true;
                                res.extend(Self::multiply(t.stream(), range.clone()));
                                continue;
                            }
                        }
                    }
                    let (group, is_found_inside) =
                        Self::expand_iter(t.stream().into_iter().peekable(), range.clone());

                    is_found = is_found_inside;
                    let mut group = Group::new(d, group);
                    group.set_span(t.span());
                    res.extend(group.to_token_stream())
                }
                TokenTree::Punct(t) => {
                    if t.as_char() == '#' {
                        if let Some(TokenTree::Group(g)) = iter.peek() {
                            if g.delimiter() == Delimiter::Parenthesis {
                                start_expansion = true;
                                continue;
                            }
                        }
                        res.extend(t.to_token_stream());
                    } else if t.as_char() == '*' && start_expansion {
                        start_expansion = false;
                    } else {
                        start_expansion = false;
                        res.extend(t.to_token_stream());
                    }
                }
            }
        }

        (res, is_found)
    }

    fn expand(mut self) -> Self {
        let (content, is_expansion_found) =
            Self::expand_iter(self.content.into_iter().peekable(), self.range.clone());

        self.content = if is_expansion_found {
            content
        } else {
            Self::multiply(content, self.range.clone())
        };
        // println!("{:#?}", self.content);
        self
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let loop_info = parse_macro_input!(input as Loop);
    let c = loop_info.content;

    // println!("{:#?}", c);
    quote! { #c }.into()
}
