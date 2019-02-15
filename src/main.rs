use std::cell::RefCell;
use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::io::stdout;
use std::rc::Rc;

mod operators;
mod structures;

use operators::*;
use structures::*;

mod minilisp_grammar {
    include!(concat!(env!("OUT_DIR"), "/minilisp_grammar.rs"));
}

fn repl() -> std::io::Result<()> {
    println!("mini-lisp interpreter");
    println!("Type :help to show repl commands.");

    let stdin = io::stdin();

    let env = Rc::new(RefCell::new(Env::new()));
    set_builtin_operator(&env);

    let mut line = String::new();
    let mut line_count = 0;
    let mut nest_count = 0;

    loop {
        if nest_count == 0 {
            line.clear();
            line_count += 1;
            print!("[{:03}] >>> ", line_count);
            stdout().flush()?;
        } else {
            print!("      ... ");
            stdout().flush()?;
        }
        let mut tmp = String::new();
        stdin.lock().read_line(&mut tmp)?;

        for x in tmp.chars() {
            match x {
                '(' => nest_count += 1,
                ')' => nest_count -= 1,
                _ => {}
            };
        }

        match tmp.as_str() {
            ":help\n" => {
                println!(":help  ---  show help");
                println!(":exit  ---  exit");
                continue;
            }
            ":exit\n" => {
                break;
            }
            "\n" => {
                continue;
            }
            _ => {}
        };

        line.push_str(&tmp);

        if nest_count != 0 {
            continue;
        }

        match minilisp_grammar::program(&line) {
            Ok(r) => match eval(&Elem::List(r), &env) {
                Ok(res) => println!("{:?}", res),
                Err(message) => println!("evaluation error: {}", message),
            },
            Err(e) => println!("parsing error: {}", e),
        };
    }
    Ok(())
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        repl()?;
    } else {
        let code = if args[1] == "-c" && args.len() >= 3 {
            // 2番目の引数を評価
            args[2].clone()
        } else {
            // ファイルから読み込む
            let mut tmp = String::new();
            let mut file = File::open(&args[1])?;
            file.read_to_string(&mut tmp)?;
            tmp
        };

        let env = Rc::new(RefCell::new(Env::new()));
        set_builtin_operator(&env);
        match minilisp_grammar::program(&code) {
            Ok(r) => match eval(&Elem::List(r), &env) {
                Ok(_) => {}
                Err(message) => println!("evaluation error: {}", message),
            },
            Err(e) => println!("parsing error: {}", e),
        };
    }
    Ok(())
}

#[test]
fn eval_test() {
    let testcase = vec![
        r#"(mul (add 1 2) (sub 6 4))"#,
        r#"(div (add 1 2 3 4 5 6) (sub 6 4))"#,
        r#"(eq (add 1 2) 3)"#,
        r#"(eq true true)"#,
        r#"(eq false true)"#,
        r#"(eq "1" 1)"#,
        r#"(not (eq "abc" "abc"))"#,
        r#"(progn (defun square (n) (mul n n)) (square 123))"#,
        r#"(progn (setq square (lambda (n) (mul n n))) (square 123))"#,
        r#"(progn (defun fib (n) (if (eq n 0) 0 (if (eq n 1) 1 (add (fib (sub n 1)) (fib (sub n 2)))))) (fib 20))"#,
        r#"(progn (defun f (n) (add n (progn (setq n 12345) n))) (f 1))"#,
        r#"(progn (defun f (n) (add (progn (setq n 12345) n) n)) (f 1))"#,
    ];
    let expected = vec![
        Some(Elem::Int(6)),
        Some(Elem::Int(10)),
        Some(Elem::Bool(true)),
        Some(Elem::Bool(true)),
        Some(Elem::Bool(false)),
        None,
        Some(Elem::Bool(false)),
        Some(Elem::Int(15129)),
        Some(Elem::Int(15129)),
        Some(Elem::Int(6765)),
        Some(Elem::Int(12346)),
        Some(Elem::Int(24690)),
    ];

    let env = Rc::new(RefCell::new(Env::new()));
    set_builtin_operator(&env);
    for (i, case) in testcase.iter().enumerate() {
        let code = case;
        let e = match minilisp_grammar::program(code) {
            Ok(r) => match eval(&Elem::List(r), &env) {
                Ok(elem) => Some(elem),
                Err(message) => {
                    println!("evaluation error: {}", message);
                    None
                }
            },
            Err(e) => {
                println!("parsing error: {}", e);
                None
            }
        };
        assert!(e == expected[i]);
    }
}
