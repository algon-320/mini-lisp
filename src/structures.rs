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
    pub fn type_name(&self) -> &str {
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
    pub fn extract_list(&self) -> Result<List, String> {
        match self {
            Elem::List(l) => Ok(l.clone()),
            _ => Err(format!(
                "mismatch type: expected 'List' but found '{}'",
                self.type_name()
            )),
        }
    }
    pub fn extract_symbol(&self) -> Result<String, String> {
        match self {
            Elem::Symbol(s) => Ok(s.clone()),
            _ => Err(format!(
                "mismatch type: expected 'Symbol' but found '{}'",
                self.type_name()
            )),
        }
    }
    pub fn extract_int(&self) -> Result<i64, String> {
        match self {
            Elem::Int(n) => Ok(n.clone()),
            _ => Err(format!(
                "mismatch type: expected 'Int' but found '{}'",
                self.type_name()
            )),
        }
    }
    pub fn extract_bool(&self) -> Result<bool, String> {
        match self {
            Elem::Bool(f) => Ok(f.clone()),
            _ => Err(format!(
                "mismatch type: expected 'Bool' but found '{}'",
                self.type_name()
            )),
        }
    }
    pub fn extract_string(&self) -> Result<String, String> {
        match self {
            Elem::StringData(s) => Ok(s.clone()),
            _ => Err(format!(
                "mismatch type: expected 'String' but found '{}'",
                self.type_name()
            )),
        }
    }
    pub fn extract_func(&self) -> Result<(List, List, Rc<RefCell<Env>>), String> {
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
    pub data: Vec<Elem>,
}

impl List {
    pub fn unit() -> List {
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

#[derive(Clone)]
pub struct Env {
    pub parent: Weak<RefCell<Env>>,
    pub vars: HashMap<String, Elem>,
}

impl PartialEq for Env {
    fn eq(&self, other: &Env) -> bool {
        self.vars == other.vars
    }
}

impl Env {
    pub fn new() -> Env {
        Env {
            parent: Weak::new(),
            vars: HashMap::new(),
        }
    }
}
