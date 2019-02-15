use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::rc::Weak;

pub type Program = List;

#[derive(Clone, PartialEq)]
pub enum Elem {
    List(List),
    Symbol(String),
    Int(i64),
    Bool(bool),
    StringData(String),
    Func(List, List, Rc<RefCell<Env>>),
    BuiltinFunc(String),
}
impl fmt::Debug for Elem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Elem::List(l) => write!(f, "{:?}", l),
            Elem::Symbol(s) => write!(f, "sym:{}", s),
            Elem::Int(n) => write!(f, "num:{}", n),
            Elem::Bool(v) => if *v {
                write!(f, "bool:true")
            } else {
                write!(f, "bool:false")
            },
            Elem::StringData(s) => write!(f, "str:{}", s),
            Elem::Func(_, _, _) => write!(f, "func: {}", "(details of func)"),
            Elem::BuiltinFunc(name) => write!(f, "builin func: {}", name),
        }
    }
}

impl Elem {
    fn type_name(&self) -> &str {
        match self {
            Elem::List(_) => "List",
            Elem::Int(_) => "Int",
            Elem::Symbol(_) => "Symbol",
            Elem::Bool(_) => "Bool",
            Elem::StringData(_) => "String",
            Elem::Func(_, _, _) => "Func",
            Elem::BuiltinFunc(_) => "BuiltinFunc",
            _ => "(undefined)",
        }
    }
    fn extract_list(&self) -> Result<List, String> {
        match self {
            Elem::List(l) => Ok(l.clone()),
            _ => Err(format!(
                "mismatch type: expected 'List' but found '{}'",
                self.type_name()
            )),
        }
    }
    fn extract_symbol(&self) -> Result<String, String> {
        match self {
            Elem::Symbol(s) => Ok(s.clone()),
            _ => Err(format!(
                "mismatch type: expected 'Symbol' but found '{}'",
                self.type_name()
            )),
        }
    }
    fn extract_int(&self) -> Result<i64, String> {
        match self {
            Elem::Int(n) => Ok(n.clone()),
            _ => Err(format!(
                "mismatch type: expected 'Int' but found '{}'",
                self.type_name()
            )),
        }
    }
    fn extract_bool(&self) -> Result<bool, String> {
        match self {
            Elem::Bool(f) => Ok(f.clone()),
            _ => Err(format!(
                "mismatch type: expected 'Bool' but found '{}'",
                self.type_name()
            )),
        }
    }
    fn extract_string(&self) -> Result<String, String> {
        match self {
            Elem::StringData(s) => Ok(s.clone()),
            _ => Err(format!(
                "mismatch type: expected 'String' but found '{}'",
                self.type_name()
            )),
        }
    }
    fn extract_func(&self) -> Result<(List, List, Rc<RefCell<Env>>), String> {
        match self {
            Elem::Func(a, b, c) => Ok((a.clone(), b.clone(), c.clone())),
            _ => Err(format!(
                "mismatch type: expected 'Func' but found '{}'",
                self.type_name()
            )),
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

mod minilisp_grammar {
    include!(concat!(env!("OUT_DIR"), "/minilisp_grammar.rs"));
}

#[derive(Clone)]
pub struct Env {
    parent: Weak<RefCell<Env>>,
    vars: HashMap<String, Elem>,
}

impl PartialEq for Env {
    fn eq(&self, other: &Env) -> bool {
        self.vars == other.vars
    }
}

impl Env {
    fn new() -> Env {
        Env {
            parent: Weak::new(),
            vars: HashMap::new(),
        }
    }
}

fn check_arg_num(args: &[Elem], n: usize) -> Option<String> {
    if args.len() != n {
        return Some(format!("mismatch number of operands."));
    }
    None
}

// args の要素をすべて足す (IntでないElemがあるとエラー)
fn op_add(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    let mut ret = 0;
    for e in args {
        let e = eval(e, env)?;
        let n = e.extract_int()?;
        ret += n;
    }
    Ok(Elem::Int(ret))
}

// args の最初の要素から他をすべて引く (IntでないElemがあるとエラー)
fn op_sub(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    let mut ret = 0;
    for (i, e) in args.iter().enumerate() {
        let e = eval(e, env)?;
        let n = e.extract_int()?;
        if i == 0 {
            ret = n;
        } else {
            ret -= n;
        }
    }
    Ok(Elem::Int(ret))
}

// args の要素をすべて掛ける (IntでないElemがあるとエラー)
fn op_mul(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    let mut ret = 1;
    for e in args {
        let e = eval(e, env)?;
        let n = e.extract_int()?;
        ret *= n;
    }
    Ok(Elem::Int(ret))
}

// args の最初の要素から他をすべて割る (IntでないElemがあるとエラー)
fn op_div(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    let mut ret = 0;
    for (i, e) in args.iter().enumerate() {
        let e = eval(e, env)?;
        let n = e.extract_int()?;
        if i == 0 {
            ret = n;
        } else {
            ret /= n;
        }
    }
    Ok(Elem::Int(ret))
}

fn op_setq(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    if let Some(e) = check_arg_num(args, 2) {
        return Err(format!("setq: {}", e));
    }
    let e1 = &args[0];
    let e2 = eval(&args[1], env)?;
    let sym = e1.extract_symbol()?;
    let mut env = env.borrow_mut();
    env.vars.insert(sym, e2.clone());
    Ok(Elem::List(List::unit()))
}

fn op_progn(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    let mut res = Elem::List(List::unit());
    for e in args {
        res = eval(e, env)?;
    }
    Ok(res)
}

fn op_eq(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    if let Some(e) = check_arg_num(args, 2) {
        return Err(format!("eq: {}", e));
    }
    let e1 = eval(&args[0], env)?;
    let e2 = eval(&args[1], env)?;
    let ret = match (e1, e2) {
        (Elem::List(l1), Elem::List(l2)) => l1 == l2,
        (Elem::Symbol(v1), Elem::Symbol(v2)) => v1 == v2,
        (Elem::Int(v1), Elem::Int(v2)) => v1 == v2,
        (Elem::Bool(v1), Elem::Bool(v2)) => v1 == v2,
        (Elem::StringData(v1), Elem::StringData(v2)) => v1 == v2,
        _ => return Err(format!("eq: mismatch types.")),
    };
    Ok(Elem::Bool(ret))
}
fn op_neq(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    if let Some(e) = check_arg_num(args, 2) {
        return Err(format!("neq: {}", e));
    }
    if let Elem::Bool(f) = op_eq(args, env)? {
        return Ok(Elem::Bool(!f));
    } else {
        panic!("fatal processing error: op_not")
    }
}
fn op_not(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    if let Some(e) = check_arg_num(args, 1) {
        return Err(format!("not: {}", e));
    }
    let e = eval(&args[0], env)?;
    if let Elem::Bool(f) = e {
        return Ok(Elem::Bool(!f));
    } else {
        return Err(format!("not: mismatch type."));
    }
}

fn op_if(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    if let Some(e) = check_arg_num(args, 3) {
        return Err(format!("if: {}", e));
    }
    let e1 = eval(&args[0], env)?;
    let f = e1.extract_bool()?;
    if f {
        eval(&args[1], env)
    } else {
        eval(&args[2], env)
    }
}

fn op_print(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    if let Some(e) = check_arg_num(args, 1) {
        return Err(format!("print: {}", e));
    }
    let e = eval(&args[0], env)?;
    match e {
        Elem::List(l) => {
            print!("(");
            for (i, e) in l.data.iter().enumerate() {
                print!("{:?}", e);
                if i != l.data.len() - 1 {
                    print!(" ");
                }
            }
            print!(")")
        }
        Elem::Symbol(_) => return Err(format!("undefined symbol.")),
        Elem::Int(n) => print!("{}", n),
        Elem::Bool(f) => print!("{:?}", f),
        Elem::StringData(s) => print!("{}", s),
        _ => print!("op_print: unimplemented: {:?}", e),
    }
    Ok(Elem::List(List::unit()))
}
fn op_println(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    if let Some(e) = check_arg_num(args, 1) {
        return Err(format!("print: {}", e));
    }
    op_print(args, env)?;
    println!();
    Ok(Elem::List(List::unit()))
}

fn op_lambda(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    if let Some(e) = check_arg_num(args, 2) {
        return Err(format!("lambda: {}", e));
    }
    let closure = Env {
        parent: Rc::downgrade(&env),
        vars: HashMap::new(),
    };
    let lambda_arg = args[0].extract_list()?;
    let ops = args[1].extract_list()?;
    Ok(Elem::Func(lambda_arg, ops, Rc::new(RefCell::new(closure))))
}

fn op_defun(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    if let Some(e) = check_arg_num(args, 3) {
        return Err(format!("defun: {}", e));
    }
    let sym = match &args[0] {
        Elem::Symbol(s) => s,
        _ => return Err(format!("defun: mismatch operand type.")),
    };
    let func = op_lambda(&args[1..], env)?;
    let mut env = env.borrow_mut();
    env.vars.insert(sym.clone(), func.clone());
    Ok(Elem::List(List::unit()))
}

fn set_builtin_operator(env: &Rc<RefCell<Env>>) {
    let op_names = [
        "defun", "lambda", "progn", "setq", "if", "eq", "neq", "not", "land", "lor", "add", "sub",
        "mul", "div", "bit_and", "bit_or", "bit_xor", "print", "println",
    ];
    let mut env = env.borrow_mut();
    for name in op_names.iter() {
        env.vars
            .insert(name.to_string(), Elem::BuiltinFunc(name.to_string()));
    }
}

fn call_builtin_operator(
    name: String,
    args: &[Elem],
    env: &Rc<RefCell<Env>>,
) -> Result<Elem, String> {
    match name.as_str() {
        "defun" => op_defun(args, env),
        "lambda" => op_lambda(args, env),
        "progn" => op_progn(args, env),
        "setq" => op_setq(args, env),
        "if" => op_if(args, env),
        "eq" => op_eq(args, env),
        "neq" => op_neq(args, env),
        "not" => op_not(args, env),
        "add" => op_add(args, env),
        "sub" => op_sub(args, env),
        "mul" => op_mul(args, env),
        "div" => op_div(args, env),
        "print" => op_print(args, env),
        "println" => op_println(args, env),
        _ => Err(format!("call_builtin_operator: undefined")),
    }
}

fn eval(elem: &Elem, env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    match elem {
        Elem::List(list) => {
            if list.data.is_empty() {
                return Ok(Elem::List(List::unit()));
            }

            let h = eval(&list.data[0], env)?;
            let ret = match h {
                Elem::BuiltinFunc(name) => call_builtin_operator(name, &list.data[1..], env)?,
                Elem::Func(args, ops, def_env) => {
                    if list.data[1..].len() != args.data.len() {
                        return Err(format!("func: mismatch number of arguments."));
                    }

                    // 引数の評価
                    let mut evalated = List { data: Vec::new() };
                    for (i, e) in list.data[1..].iter().enumerate() {
                        evalated.data.push(eval(&e, &env)?);
                    }

                    let mut call_env = Env {
                        parent: Rc::downgrade(&def_env),
                        vars: HashMap::new(),
                    };

                    // 引数を環境に登録
                    {
                        // let env = env.borrow_mut();
                        for (i, e) in args.data.iter().enumerate() {
                            match e {
                                Elem::Symbol(name) => {
                                    call_env.vars.insert(name.clone(), evalated.data[i].clone());
                                }
                                _ => return Err(format!("func: mismatch type of arguments")),
                            };
                        }
                    }
                    // 呼び出し
                    eval(&Elem::List(ops), &Rc::new(RefCell::new(call_env)))?
                }
                _ => Elem::List(list.clone()),
            };
            Ok(ret)
        }
        Elem::Symbol(sym) => {
            let env = env.borrow();
            if let Some(e) = env.vars.get(sym) {
                return Ok(e.clone());
            }
            // 親スコープを探索
            let mut cur = env.parent.clone();
            loop {
                if let Some(e) = cur.upgrade() {
                    let tmp = e.borrow();
                    match tmp.vars.get(sym) {
                        Some(elem) => return Ok(elem.clone()),
                        None => cur = tmp.parent.clone(),
                    };
                } else {
                    break;
                }
            }
            Err(format!("eval: undefined symbol: {}", sym))
        }
        _ => Ok(elem.clone()),
    }
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
    (fib 30)
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
fn eval_arithmatic() {
    let testcase = vec![
        r#"(mul (add 1 2) (sub 6 4))"#,
        r#"(div (add 1 2 3 4 5 6) (sub 6 4))"#,
    ];
    let expected = vec![Some(Elem::Int(6)), Some(Elem::Int(10))];

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
        Some(Elem::Bool(true)),
        Some(Elem::Bool(true)),
        Some(Elem::Bool(false)),
        None,
        Some(Elem::Bool(false)),
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
