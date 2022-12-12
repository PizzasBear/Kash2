use std::{fs, path::Path, sync::Arc};

use kash2::{lexer2, parser2};
use kash2_derive::mamamia;

fn parse_int(
    mut tokens: lexer2::TokensRef,
) -> parser2::Result<(&lexer2::IntLiteral, lexer2::TokensRef)> {
    mamamia!(#name:int);
    Ok((name, tokens))
}

fn parse_add(
    mut tokens: lexer2::TokensRef,
) -> parser2::Result<((u64, lexer2::Span), lexer2::TokensRef)> {
    mamamia! {
        $(#a:(parse_int) + #b:(parse_int))@span;
    };

    println!("({a:?}) + ({b:?}): {}", span.show());

    Ok(((a.value + b.value, span), tokens))
}

#[test]
fn testing() -> anyhow::Result<()> {
    let file = Path::new("./f.ks2");

    let tokens = lexer2::parse_file(Arc::new(file.to_owned()), &fs::read_to_string(file)?)
        .expect("Failed to tokenize");
    let mut tokens_ref = tokens.as_ref();

    let (value, span);
    ((value, span), tokens_ref) = parse_add(tokens_ref)?;
    println!("Add {value}: {}", span.show());
    println!("We've got the following left over: {:#?}", tokens_ref);

    Ok(())
}
