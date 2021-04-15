use std::io::{stdin, Write};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassManager,
    values::{FunctionValue, IntValue},
};

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

// sample if ( iszero 0 ) then if ( ( iszero succ succ 0 ) then ( 0 ) else ( pred pred succ 0 ) ) else 0 == -1
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
                            compiler_fn(&ast).unwrap();
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

fn compiler_fn(ast: &Ast) -> Result<(), &'static str> {
    let context = Context::create();
    let builder = context.create_builder();
    let module = context.create_module("repl");

    let fpm = PassManager::create(&module);

    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();

    fpm.initialize();

    let ty = match execute(ast).unwrap() {
        ExecutionResult::Number(_) => context.i64_type(),
        ExecutionResult::Boolean(_) => context.bool_type(),
    };

    let fn_type = ty.fn_type(&[], false);
    let fn_val = module.add_function(
        &format!("anon_func_{}", rand::random::<u64>()),
        fn_type,
        None,
    );

    let entry = context.append_basic_block(fn_val, "entry");
    builder.position_at_end(entry);
    let expr = compile_expr(&context, &module, &builder, &fn_val, ast)?;
    builder.build_return(Some(&expr));

    if fn_val.verify(true) {
        fpm.run_on(&fn_val);
        fn_val.print_to_stderr();
        Ok(())
    } else {
        Err("Caused unknown error")
    }
}

fn compile_expr<'ctx>(
    context: &'ctx Context,
    module: &'ctx Module,
    builder: &'ctx Builder,
    func: &'ctx FunctionValue,
    ast: &Ast,
) -> Result<IntValue<'ctx>, &'static str> {
    let i = match ast {
        Ast::True => context.bool_type().const_int(1, true),
        Ast::False => context.bool_type().const_int(0, true),
        Ast::Conditional(cond, t, f) => match execute(cond).unwrap() {
            ExecutionResult::Number(_) => panic!("Type Error!"),
            ExecutionResult::Boolean(_) => {
                let cond = compile_expr(context, module, builder, func, cond)?;
                let cond = builder.build_int_compare(
                    inkwell::IntPredicate::EQ,
                    cond,
                    context.bool_type().const_int(1, true),
                    "ifcond",
                );

                let t_ty = match execute(t).unwrap() {
                    ExecutionResult::Number(_) => context.i64_type(),
                    ExecutionResult::Boolean(_) => context.bool_type(),
                };

                let then_bb = context.append_basic_block(*func, "if_then");
                let else_bb = context.append_basic_block(*func, "if_else");
                let cont_bb = context.append_basic_block(*func, "if_cont");

                builder.build_conditional_branch(cond, then_bb, else_bb);

                builder.position_at_end(then_bb);
                let then_val = compile_expr(context, module, builder, func, t)?;
                builder.build_unconditional_branch(cont_bb);
                let then_bb = builder.get_insert_block().unwrap();

                builder.position_at_end(else_bb);
                let else_val = compile_expr(context, module, builder, func, f)?;
                builder.build_unconditional_branch(cont_bb);
                let else_bb = builder.get_insert_block().unwrap();

                builder.position_at_end(cont_bb);
                let phi = builder.build_phi(t_ty, "if_tmp");
                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);
                phi.as_basic_value().into_int_value()
            }
        },
        Ast::Zero => context.i64_type().const_int(0, true),
        Ast::Succ(l) => {
            let l = compile_expr(context, module, builder, func, l)?;
            let r = rand::random::<u64>();
            builder.build_int_add(
                l,
                context.i64_type().const_int(1, true),
                &format!("add_rand_{}", r),
            )
        }
        Ast::Pred(l) => {
            let l = compile_expr(context, module, builder, func, l)?;
            let r = rand::random::<u64>();
            builder.build_int_sub(
                l,
                context.i64_type().const_int(1, true),
                &format!("sub_rand_{}", r),
            )
        }
        Ast::IsZero(a) => {
            let r = rand::random::<u64>();
            builder.build_int_compare(
                inkwell::IntPredicate::EQ,
                compile_expr(context, module, builder, func, a)?,
                context.i64_type().const_int(0, true),
                &format!("is_zero_{}", r),
            )
        }
    };
    Ok(i)
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
            "(" | ")" => {}
            _ => return Err(format!("Invalid token:{}", atom)),
        }
    }
    Ok(items)
}
