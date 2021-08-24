extern crate proc_macro;
use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::*;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;

struct ArgumentList(Vec<Expr>);

impl Parse for ArgumentList {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut exprs = vec![];
        let lookahead = input.lookahead1();
        let k: LitInt = input.parse()?;
        let c: Token![*] = input.parse()?;
        let c: Token![,] = input.parse()?;
        let a: Punctuated<Expr, Token![,]> = input.parse_terminated(Parse::parse)?;
        for i in a {
            exprs.push(i);
        }
        Ok(ArgumentList(exprs))
    }
}

#[proc_macro]
pub fn make_answer(item: TokenStream) -> TokenStream {
    let ast : ArgumentList = syn::parse(item).unwrap();
    (quote!{
        fn pepega() -> i32 {
            42
        }
    }).into()
}
