use std::io::{stdin, stdout, Write};

pub fn start() {
    let mut line = String::new();
    let t = 43u64;
    let s = std::ptr::addr_of!(t);
    loop {
        print!(">>> ");
        stdout().flush().unwrap();
        stdin().read_line(&mut line).unwrap();
        match lex(&line) {
            Ok(vec) => {
                println!("{:?}", &vec);
            }
            Err(err) => println!("{}", err),
        }
    }
}

#[derive(Debug)]
enum LexicalItem {
    Lambda,
    LeftParen,
    RightParen,
    Dot,
    Identifier(String),
}

fn lex(input: &str) -> Result<Vec<LexicalItem>, &str> {
    let mut i = 0;
    let mut tokens = Vec::new();
    let chars = input.trim_end().chars().collect::<Vec<_>>();
    let len = chars.len();
    loop {
        let current = chars[i];
        let has_next = i != len - 1;
        let mut prog = 1;
        match current {
            '\\' => {
                tokens.push(LexicalItem::Lambda);
            }
            '(' => tokens.push(LexicalItem::LeftParen),
            ')' => tokens.push(LexicalItem::RightParen),
            'a'..='z' | 'A'..='Z' => {
                let mut s = String::new();
                let mut j = 0;
                loop {
                    let next = chars[i + j];
                    if next.is_ascii_alphabetic() {
                        s.push(next);
                    } else {
                        break;
                    }
                    if i + j == len - 1 {
                        break;
                    }
                    j += 1;
                }
                prog = s.len();
                tokens.push(LexicalItem::Identifier(s));
            }
            '.' => tokens.push(LexicalItem::Dot),
            ' ' | '\t' => {}
            _ => return Err("Unknown token"),
        }

        i += prog;
        if !has_next {
            break;
        }
    }

    Ok(tokens)
}
