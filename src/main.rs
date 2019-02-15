use std::cell::RefCell;
use std::rc::Rc;

mod operators;
mod structures;

use operators::*;
use structures::*;

mod minilisp_grammar {
    include!(concat!(env!("OUT_DIR"), "/minilisp_grammar.rs"));
}

fn main() {
    let code = r#"(progn
    (defun fib (n)
        (if (eq n 0) 0
            (if (eq n 1) 1
                (add (fib (sub n 1)) (fib (sub n 2)))
            )
        )
    )
    (fib 20)
    )"#;

    let env = Rc::new(RefCell::new(Env::new()));
    set_builtin_operator(&env);
    match minilisp_grammar::program(code) {
        Ok(r) => match eval(&Elem::List(r), &env) {
            Ok(elem) => println!("> {:?}", elem),
            Err(message) => {
                println!("evaluation error: {}", message);
            }
        },
        Err(e) => {
            println!("parsing error: {}", e);
        }
    }
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
