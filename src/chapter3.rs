use std::io::{stdin, Write};

// 型無し算術式（でも型検査してる😂）

enum Ast {
    True,
    False,
    Conditional(Box<Ast>, Box<Ast>, Box<Ast>),
    Zero,
    Succ(Box<Ast>),
    Pred(Box<Ast>),
    IsZero(Box<Ast>),
}

#[derive(Debug)]
enum LexicalItem {
    True,
    False,
    If,
    Then,
    Else,
    Zero,
    Succ,
    Pred,
    IsZero,
}

#[derive(Debug)]
enum ExecutionResult {
    Number(i32),
    Boolean(bool),
}

impl std::fmt::Display for ExecutionResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ExecutionResult::Boolean(b) => b.to_string(),
                ExecutionResult::Number(n) => n.to_string(),
            }
        )
    }
}

pub fn start() {
    let mut line = String::new();
    loop {
        print!(">>> ");
        std::io::stdout().flush().unwrap();
        stdin().read_line(&mut line).unwrap();
        // println!("{}", line.trim_end());
        match lex(&line) {
            Ok(mut vec) => {
                // vec.iter().for_each(|x| println!("{:?}, ", x));
                vec.reverse();
                match parse(&mut vec) {
                    Ok(ast) => match execute(&ast) {
                        Ok(result) => {
                            println!("{}", result);
                        }
                        Err(err) => println!("{}", err),
                    },
                    Err(err) => println!("{}", err),
                }
            }
            Err(err) => println!("{}", err),
        }
        line.clear();
    }
}

// big step style
fn execute(ast: &Ast) -> Result<ExecutionResult, &'static str> {
    match ast {
        Ast::False => Ok(ExecutionResult::Boolean(false)),
        Ast::True => Ok(ExecutionResult::Boolean(true)),
        Ast::Zero => Ok(ExecutionResult::Number(0)),
        Ast::Succ(child) | Ast::Pred(child) => {
            let ans = execute(child.as_ref())?;
            match ans {
                ExecutionResult::Number(n) => {
                    Ok(ExecutionResult::Number(if let Ast::Succ(_) = ast {
                        n + 1
                    } else {
                        n - 1
                    }))
                }
                ExecutionResult::Boolean(_) => Err("Type Error"),
            }
        }
        Ast::IsZero(child) => {
            let ans = execute(child.as_ref())?;
            match ans {
                ExecutionResult::Number(0) => Ok(ExecutionResult::Boolean(true)),
                ExecutionResult::Number(_) => Ok(ExecutionResult::Boolean(false)),
                ExecutionResult::Boolean(_) => Err("Type Error"),
            }
        }
        Ast::Conditional(condition, first, second) => {
            let condition_ans = execute(condition.as_ref())?;
            match condition_ans {
                ExecutionResult::Number(_) => Err("Type error"),
                ExecutionResult::Boolean(true) => execute(first.as_ref()),
                ExecutionResult::Boolean(false) => execute(second.as_ref()),
            }
        }
    }
}

fn parse(vec: &mut Vec<LexicalItem>) -> Result<Ast, String> {
    if let Some(item) = vec.last() {
        match item {
            LexicalItem::True => {
                vec.pop();
                Ok(Ast::True)
            }
            LexicalItem::False => {
                vec.pop();
                Ok(Ast::False)
            }
            LexicalItem::If => parse_if_then_else(vec),
            LexicalItem::Zero => {
                vec.pop();
                Ok(Ast::Zero)
            }
            LexicalItem::Succ | LexicalItem::Pred | LexicalItem::IsZero => parse_unary(vec),
            _ => Err(format!("invalid syntax near {:?}", item)),
        }
    } else {
        Err(String::from("no input"))
    }
}

fn parse_if_then_else(vec: &mut Vec<LexicalItem>) -> Result<Ast, String> {
    let mut has_error_pos = None;
    let mut ast = None;
    if let Some(LexicalItem::If) = vec.pop() {
        let condition = parse(vec)?;
        if let Some(LexicalItem::Then) = vec.pop() {
            let then_exp = parse(vec)?;
            if let Some(LexicalItem::Else) = vec.pop() {
                let else_exp = parse(vec)?;
                ast = Some(Ast::Conditional(
                    Box::new(condition),
                    Box::new(then_exp),
                    Box::new(else_exp),
                ))
            } else {
                has_error_pos = Some(LexicalItem::Else);
            }
        } else {
            has_error_pos = Some(LexicalItem::Then);
        }
    } else {
        has_error_pos = Some(LexicalItem::If);
    }
    if let Some(e) = has_error_pos {
        Err(format!("invalid syntax near {:?}", e))
    } else {
        Ok(ast.unwrap())
    }
}

fn parse_unary(vec: &mut Vec<LexicalItem>) -> Result<Ast, String> {
    let first = vec.pop();
    if first.is_none() {
        Err(String::from("invalid token"))
    } else if let Some(t) = first {
        match t {
            LexicalItem::Succ | LexicalItem::Pred | LexicalItem::IsZero => {
                let t2 = parse(vec)?;
                match t {
                    LexicalItem::Succ => Ok(Ast::Succ(Box::new(t2))),
                    LexicalItem::Pred => Ok(Ast::Pred(Box::new(t2))),
                    LexicalItem::IsZero => Ok(Ast::IsZero(Box::new(t2))),
                    _ => panic!(),
                }
            }
            _ => Err(format!("invalid syntax near {:?}", t)),
        }
    } else {
        panic!();
    }
}

fn lex(input: &str) -> Result<Vec<LexicalItem>, String> {
    let split = input.split_ascii_whitespace();
    let mut items = Vec::new();
    for atom in split {
        match atom {
            "true" => items.push(LexicalItem::True),
            "false" => items.push(LexicalItem::False),
            "if" => items.push(LexicalItem::If),
            "then" => items.push(LexicalItem::Then),
            "else" => items.push(LexicalItem::Else),
            "0" => items.push(LexicalItem::Zero),
            "succ" => items.push(LexicalItem::Succ),
            "pred" => items.push(LexicalItem::Pred),
            "iszero" => items.push(LexicalItem::IsZero),
            _ => return Err(format!("Invalid token:{}", atom)),
        }
    }
    Ok(items)
}
