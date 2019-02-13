use std::collections::HashMap;
use std::fmt;

pub type Program = List;

#[derive(Clone, PartialEq)]
pub enum Elem {
    ListElement(List),
    AtomElement(Atom),
}
impl fmt::Debug for Elem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Elem::ListElement(l) => write!(f, "{:?}", l),
            Elem::AtomElement(a) => write!(f, "{:?}", a),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct List {
    data: Vec<Elem>,
}

impl List {
    fn unit() -> List {
        List { data: Vec::new() }
    }
}

impl fmt::Debug for List {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(");
        for (i, e) in self.data.iter().enumerate() {
            write!(f, "{:?}", e);
            if i != self.data.len() - 1 {
                write!(f, " ");
            }
        }
        write!(f, ")")
    }
}

#[derive(Clone, PartialEq)]
pub enum Atom {
    Symbol(String),
    Int(i64),
    Bool(bool),
    StringData(String),
}

impl fmt::Debug for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Atom::Symbol(s) => write!(f, "sym:{}", s),
            Atom::Int(n) => write!(f, "num:{}", n),
            Atom::Bool(v) => if *v {
                write!(f, "bool:true")
            } else {
                write!(f, "bool:false")
            },
            Atom::StringData(s) => write!(f, "str:{}", s),
        }
    }
}

mod minilisp_grammar {
    include!(concat!(env!("OUT_DIR"), "/minilisp_grammar.rs"));
}

struct Env {
    vars: HashMap<String, Elem>,
}

impl Env {
    fn new() -> Env {
        Env {
            vars: HashMap::new(),
        }
    }
}

fn extract_int(elem: &Elem) -> Option<i64> {
    match elem {
        Elem::AtomElement(a) => match a {
            Atom::Int(n) => Some(n.clone()),
            _ => return None,
        },
        _ => return None,
    }
}

// list の要素をすべて足す (IntでないElemがあるとエラー)
fn op_add(list: &List, env: &mut Env) -> Result<Elem, String> {
    let mut ret = 0;
    for e in &list.data {
        let e = eval(e, env)?;
        if let Some(n) = extract_int(&e) {
            ret += n;
        } else {
            return Err(format!("mismatched types."));
        }
    }
    Ok(Elem::AtomElement(Atom::Int(ret)))
}

// list の最初の要素から他をすべて引く (IntでないElemがあるとエラー)
fn op_sub(list: &List, env: &mut Env) -> Result<Elem, String> {
    let mut ret = 0;
    for (i, e) in list.data.iter().enumerate() {
        let e = eval(e, env)?;
        if let Some(n) = extract_int(&e) {
            if i == 0 {
                ret = n;
            } else {
                ret -= n;
            }
        } else {
            return Err(format!("mismatched types."));
        }
    }
    Ok(Elem::AtomElement(Atom::Int(ret)))
}

// list の要素をすべて掛ける (IntでないElemがあるとエラー)
fn op_mul(list: &List, env: &mut Env) -> Result<Elem, String> {
    let mut ret = 1;
    for e in &list.data {
        let e = eval(e, env)?;
        if let Some(n) = extract_int(&e) {
            ret *= n;
        } else {
            return Err(format!("mismatched types."));
        }
    }
    Ok(Elem::AtomElement(Atom::Int(ret)))
}

// list の最初の要素から他をすべて割る (IntでないElemがあるとエラー)
fn op_div(list: &List, env: &mut Env) -> Result<Elem, String> {
    let mut ret = 0;
    for (i, e) in list.data.iter().enumerate() {
        let e = eval(e, env)?;
        if let Some(n) = extract_int(&e) {
            if i == 0 {
                ret = n;
            } else {
                ret /= n;
            }
        } else {
            return Err(format!("mismatched types."));
        }
    }
    Ok(Elem::AtomElement(Atom::Int(ret)))
}

fn op_setq(list: &List, env: &mut Env) -> Result<Elem, String> {
    if let Some(e) = check_arg_num(list, 2) {
        return Err(format!("setq: {}", e));
    }
    let e1 = eval(&list.data[0], env)?;
    let e2 = eval(&list.data[1], env)?;
    match e1 {
        Elem::AtomElement(atom) => match atom {
            Atom::Symbol(sym) => env.vars.insert(sym.clone(), e2.clone()),
            _ => return Err(format!("cannot assign to non-simbol element.")),
        },
        _ => return Err(format!("cannot assign to non-simbol element.")),
    };
    Ok(Elem::ListElement(List::unit()))
}

fn op_progn(list: &List, env: &mut Env) -> Result<Elem, String> {
    let mut res = Elem::ListElement(List::unit());
    for e in &list.data {
        res = eval(e, env)?;
    }
    Ok(res)
}

// fn op_defun(list: &List, env: &mut Env) -> Result<Elem, String> {}
fn check_arg_num(list: &List, n: usize) -> Option<String> {
    if list.data.len() != n {
        return Some(format!("mismatch number of operands."));
    }
    None
}

fn op_eq(list: &List, env: &mut Env) -> Result<Elem, String> {
    if let Some(e) = check_arg_num(list, 2) {
        return Err(format!("eq: {}", e));
    }
    let e1 = eval(&list.data[0], env)?;
    let e2 = eval(&list.data[1], env)?;
    let ret = match (e1, e2) {
        (Elem::ListElement(l1), Elem::ListElement(l2)) => l1 == l2,
        (Elem::AtomElement(a1), Elem::AtomElement(a2)) => match (a1, a2) {
            (Atom::Symbol(v1), Atom::Symbol(v2)) => v1 == v2,
            (Atom::Int(v1), Atom::Int(v2)) => v1 == v2,
            (Atom::Bool(v1), Atom::Bool(v2)) => v1 == v2,
            (Atom::StringData(v1), Atom::StringData(v2)) => v1 == v2,
            _ => return Err(format!("eq: mismatch types.")),
        },
        _ => return Err(format!("eq: mismatch types.")),
    };
    Ok(Elem::AtomElement(Atom::Bool(ret)))
}
fn op_neq(list: &List, env: &mut Env) -> Result<Elem, String> {
    if let Some(e) = check_arg_num(list, 2) {
        return Err(format!("neq: {}", e));
    }
    if let Elem::AtomElement(Atom::Bool(f)) = op_eq(list, env)? {
        return Ok(Elem::AtomElement(Atom::Bool(!f)));
    } else {
        panic!("fatal processing error: op_not")
    }
}
fn op_not(list: &List, env: &mut Env) -> Result<Elem, String> {
    if let Some(e) = check_arg_num(list, 1) {
        return Err(format!("not: {}", e));
    }
    let e = eval(&list.data[0], env)?;
    if let Elem::AtomElement(Atom::Bool(f)) = e {
        return Ok(Elem::AtomElement(Atom::Bool(!f)));
    } else {
        return Err(format!("not: mismatch type."));
    }
}

fn op_if(list: &List, env: &mut Env) -> Result<Elem, String> {
    if let Some(e) = check_arg_num(list, 3) {
        return Err(format!("if: {}", e));
    }
    let e1 = eval(&list.data[0], env)?;
    if let Elem::AtomElement(Atom::Bool(f)) = e1 {
        if f {
            return eval(&list.data[1], env);
        } else {
            return eval(&list.data[2], env);
        }
    } else {
        return Err(format!("if: mismatch type."));
    }
}

fn op_print(list: &List, env: &mut Env) -> Result<Elem, String> {
    if let Some(e) = check_arg_num(list, 1) {
        return Err(format!("print: {}", e));
    }
    let e = eval(&list.data[0], env)?;
    match e {
        Elem::ListElement(l) => {
            print!("(");
            for (i, e) in l.data.iter().enumerate() {
                print!("{:?}", e);
                if i != l.data.len() - 1 {
                    print!(" ");
                }
            }
            print!(")")
        }
        Elem::AtomElement(a) => match a {
            Atom::Symbol(_) => return Err(format!("undefined symbol.")),
            Atom::Int(n) => print!("{}", n),
            Atom::Bool(f) => print!("{:?}", f),
            Atom::StringData(s) => print!("{}", s),
        },
    }
    Ok(Elem::ListElement(List::unit()))
}
fn op_println(list: &List, env: &mut Env) -> Result<Elem, String> {
    if let Some(e) = check_arg_num(list, 1) {
        return Err(format!("print: {}", e));
    }
    op_print(list, env)?;
    println!();
    Ok(Elem::ListElement(List::unit()))
}

fn op_defun(list: &List, env: &mut Env) -> Result<Elem, String> {
    Err(format!("unimplemented"))
}

fn check_builtin_operator(name: &str) -> Option<fn(&List, &mut Env) -> Result<Elem, String>> {
    let upper = name.to_lowercase();
    match upper.as_str() {
        "defun" => Some(op_defun),
        "progn" => Some(op_progn),
        "setq" => Some(op_setq),
        "if" => Some(op_if),
        "eq" => Some(op_eq),
        "neq" => Some(op_neq),
        "not" => Some(op_not),
        "add" => Some(op_add),
        "sub" => Some(op_sub),
        "mul" => Some(op_mul),
        "div" => Some(op_div),
        "print" => Some(op_print),
        "println" => Some(op_println),
        _ => None,
    }
}

fn eval(elem: &Elem, env: &mut Env) -> Result<Elem, String> {
    match elem {
        Elem::AtomElement(atom) => match atom {
            Atom::Symbol(sym) => {
                if let Some(e) = env.vars.get(sym) {
                    return Ok(e.clone());
                }
                Ok(Elem::AtomElement(atom.clone()))
            }
            _ => Ok(Elem::AtomElement(atom.clone())),
        },
        Elem::ListElement(list) => {
            if list.data.is_empty() {
                return Ok(Elem::ListElement(List::unit()));
            }

            let h = eval(&list.data[0], env)?;
            let ret = match h {
                Elem::AtomElement(a) => match a {
                    Atom::Symbol(sym) => {
                        if let Some(f) = check_builtin_operator(&sym) {
                            match f(
                                &List {
                                    data: list.data[1..].to_vec(),
                                },
                                env,
                            ) {
                                Ok(e) => e,
                                Err(msg) => return Err(msg),
                            }
                        } else {
                            Elem::ListElement(list.clone())
                        }
                    }
                    _ => Elem::ListElement(list.clone()),
                },
                _ => Elem::ListElement(list.clone()),
            };
            Ok(ret)
        }
    }
}

fn main() {
    let code = "(progn (println true) (println false) (println (123 456 789)) (if true (print \"Hello\") (print 123)) (print \"\n\"))";

    match minilisp_grammar::program(code) {
        Ok(r) => match eval(&Elem::ListElement(r), &mut Env::new()) {
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
fn eval_arithmatic() {
    let testcase = vec![
        r#"(mul (add 1 2) (sub 6 4))"#,
        r#"(div (add 1 2 3 4 5 6) (sub 6 4))"#,
    ];
    let expected = vec![
        Some(Elem::AtomElement(Atom::Int(6))),
        Some(Elem::AtomElement(Atom::Int(10))),
    ];
    for (i, case) in testcase.iter().enumerate() {
        let code = case;
        let e = match minilisp_grammar::program(code) {
            Ok(r) => match eval(&Elem::ListElement(r), &mut Env::new()) {
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

#[test]
fn eval_logical() {
    let testcase = vec![
        r#"(eq (add 1 2) 3)"#,
        r#"(eq true true)"#,
        r#"(eq false true)"#,
        r#"(eq "1" 1)"#,
        r#"(not (eq "abc" "abc"))"#,
    ];
    let expected = vec![
        Some(Elem::AtomElement(Atom::Bool(true))),
        Some(Elem::AtomElement(Atom::Bool(true))),
        Some(Elem::AtomElement(Atom::Bool(false))),
        None,
        Some(Elem::AtomElement(Atom::Bool(false))),
    ];
    for (i, case) in testcase.iter().enumerate() {
        let code = case;
        let e = match minilisp_grammar::program(code) {
            Ok(r) => match eval(&Elem::ListElement(r), &mut Env::new()) {
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
