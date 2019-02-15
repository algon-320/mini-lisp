use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use structures::*;

pub fn check_arg_num(args: &[Elem], n: usize) -> Option<String> {
    if args.len() != n {
        return Some(format!("mismatch number of operands."));
    }
    None
}

// args の要素をすべて足す (IntでないElemがあるとエラー)
pub fn op_add(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    let mut ret = 0;
    for e in args {
        let e = eval(e, env)?;
        let n = e.extract_int()?;
        ret += n;
    }
    Ok(Elem::Int(ret))
}

// args の最初の要素から他をすべて引く (IntでないElemがあるとエラー)
pub fn op_sub(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
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
pub fn op_mul(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    let mut ret = 1;
    for e in args {
        let e = eval(e, env)?;
        let n = e.extract_int()?;
        ret *= n;
    }
    Ok(Elem::Int(ret))
}

// args の最初の要素から他をすべて割る (IntでないElemがあるとエラー)
pub fn op_div(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    let mut ret = 0;
    for (i, e) in args.iter().enumerate() {
        let e = eval(e, env)?;
        let n = e.extract_int()?;
        if i == 0 {
            ret = n;
        } else {
            if n == 0 {
                return Err(format!("div: divided by 0."));
            }
            ret /= n;
        }
    }
    Ok(Elem::Int(ret))
}

// args の最初の要素を2番目以降の要素で順に割った余り (IntでないElemがあるとエラー)
pub fn op_mod(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    let mut ret = 0;
    for (i, e) in args.iter().enumerate() {
        let e = eval(e, env)?;
        let n = e.extract_int()?;
        if i == 0 {
            ret = n;
        } else {
            if n == 0 {
                return Err(format!("mov: divided by 0."));
            }
            ret %= n;
        }
    }
    Ok(Elem::Int(ret))
}

pub fn op_setq(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
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

pub fn op_progn(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    let mut res = Elem::List(List::unit());
    for e in args {
        res = eval(e, env)?;
    }
    Ok(res)
}

pub fn op_eq(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
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
pub fn op_neq(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    if let Some(e) = check_arg_num(args, 2) {
        return Err(format!("neq: {}", e));
    }
    if let Elem::Bool(f) = op_eq(args, env)? {
        return Ok(Elem::Bool(!f));
    } else {
        panic!("fatal processing error: op_not")
    }
}
pub fn op_not(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
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

pub fn op_if(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
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

pub fn op_print(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
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
pub fn op_println(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    if let Some(e) = check_arg_num(args, 1) {
        return Err(format!("print: {}", e));
    }
    op_print(args, env)?;
    println!();
    Ok(Elem::List(List::unit()))
}

pub fn op_lambda(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
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

pub fn op_defun(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
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

pub fn op_land(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    let mut ret = true;
    for e in args {
        let e = eval(e, env)?;
        let x = e.extract_bool()?;
        ret &= x;
    }
    Ok(Elem::Bool(ret))
}
pub fn op_lor(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    let mut ret = false;
    for e in args {
        let e = eval(e, env)?;
        let x = e.extract_bool()?;
        ret |= x;
    }
    Ok(Elem::Bool(ret))
}
pub fn op_band(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    let mut ret = 0;
    for (i, e) in args.iter().enumerate() {
        let e = eval(e, env)?;
        let x = e.extract_int()?;
        if i == 0 {
            ret = x;
        } else {
            ret &= x;
        }
    }
    Ok(Elem::Int(ret))
}
pub fn op_bor(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    let mut ret = 0;
    for (i, e) in args.iter().enumerate() {
        let e = eval(e, env)?;
        let x = e.extract_int()?;
        if i == 0 {
            ret = x;
        } else {
            ret |= x;
        }
    }
    Ok(Elem::Int(ret))
}
pub fn op_bxor(args: &[Elem], env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
    let mut ret = 0;
    for (i, e) in args.iter().enumerate() {
        let e = eval(e, env)?;
        let x = e.extract_int()?;
        if i == 0 {
            ret = x;
        } else {
            ret ^= x;
        }
    }
    Ok(Elem::Int(ret))
}

pub fn set_builtin_operator(env: &Rc<RefCell<Env>>) {
    let op_names = [
        "defun", "lambda", "let", "progn", "setq", "if", "eq", "neq", "not", "land", "lor", "add",
        "sub", "mul", "div", "mod", "band", "bor", "bxor", "print", "println",
    ];
    let mut env = env.borrow_mut();
    for name in op_names.iter() {
        env.vars
            .insert(name.to_string(), Elem::BuiltinFunc(name.to_string()));
    }
}

pub fn call_builtin_operator(
    name: String,
    args: &[Elem],
    env: &Rc<RefCell<Env>>,
) -> Result<Elem, String> {
    match name.as_str() {
        "defun" => op_defun(args, env),
        "lambda" => op_lambda(args, env),
        "let" => unimplemented!(),
        "progn" => op_progn(args, env),
        "setq" => op_setq(args, env),
        "if" => op_if(args, env),
        "eq" => op_eq(args, env),
        "neq" => op_neq(args, env),
        "not" => op_not(args, env),
        "land" => op_land(args, env),
        "lor" => op_lor(args, env),
        "add" => op_add(args, env),
        "sub" => op_sub(args, env),
        "mul" => op_mul(args, env),
        "div" => op_div(args, env),
        "mod" => op_mod(args, env),
        "band" => op_band(args, env),
        "bor" => op_bor(args, env),
        "bxor" => op_bxor(args, env),
        "print" => op_print(args, env),
        "println" => op_println(args, env),
        _ => Err(format!("call_builtin_operator: undefined")),
    }
}

pub fn eval(elem: &Elem, env: &Rc<RefCell<Env>>) -> Result<Elem, String> {
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
                    for e in &list.data[1..] {
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
