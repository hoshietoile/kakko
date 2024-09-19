use std::collections::HashMap;
use std::fmt;
use std::io::prelude::*;

#[derive(Debug, Clone)]
struct Function {
    args_block: String,
    body_block: String,
}

#[derive(Debug, Clone)]
enum Vars {
    Var(Value),
    Function(Function),
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum Value {
    Num(i32),
    Bool(bool),
    List(List),
    Str(String),
    Nil,
}

struct Vm<'a> {
    input: &'a str,
    vars: Vec<HashMap<String, Vars>>,
}

impl<'a> Vm<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            vars: vec![HashMap::new()],
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct Cons {
    value: Value,
    next: List,
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum List {
    Cons(Box<Cons>),
    Nil,
}

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            List::Cons(cons) => {
                write!(f, "{} {}", cons.value, cons.next.to_string())
            }
            List::Nil => write!(f, ""),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Num(i) => write!(f, "{}", i),
            Value::Str(s) => write!(f, "{}", s),
            Value::List(lst) => write!(f, "{}", lst.to_string()),
            _ => write!(f, ""),
        }
    }
}

macro_rules! impl_op {
    {$name:ident, $op:tt} => {
        fn $name(vm: &mut Vm, input: &str) -> Value {
            let tokens = extract_n_chunks(2, input);
            let v1 = eval(vm, tokens.get(0).unwrap());
            let v2 = eval(vm, tokens.get(1).unwrap());
            match (v1, v2) {
                (Value::Num(i), Value::Num(j)) => Value::Num(i $op j),
                _ => panic!("Operator expects number."),
            }
        }
    }
}
impl_op!(add, +);
impl_op!(sub, -);
impl_op!(mul, *);
impl_op!(div, /);

fn print(value: &Value) {
    println!("{}", value.to_string());
}

fn cons(value: Value, list: List) -> List {
    match value {
        Value::Nil => List::Nil,
        _ => List::Cons(Box::new(Cons { value, next: list })),
    }
}

fn eval_block(vm: &mut Vm, block: &str) -> Value {
    let inner = extract_block_inner(block);
    let operator = extract_token(inner);
    let op_len = operator.len();
    let rest = (&inner[op_len..]).trim();

    match operator {
        "cons" => {
            let chunks = extract_n_chunks(2, rest);
            let arg1 = chunks.get(0).unwrap();
            let arg2 = chunks.get(1).unwrap();
            if arg1.is_empty() {
                return Value::List(List::Nil);
            }
            let v1 = eval(vm, arg1);
            if arg2.len() > 0 {
                let v2 = eval(vm, arg2);
                match v2 {
                    Value::List(lst) => Value::List(cons(v1, lst)),
                    _ => panic!("{v2:?} is not a list."),
                }
            } else {
                Value::List(cons(v1, List::Nil))
            }
        }
        "car" => {
            let (arg, _) = extract_next_chunk_with_rest(rest);
            let lst = eval(vm, arg);
            match lst {
                Value::List(List::Cons(lst)) => lst.value,
                Value::List(List::Nil) => panic!("{arg:?} is a empty list."),
                _ => panic!("{arg:?} is not a list"),
            }
        }
        "cdr" => {
            let (arg, _) = extract_next_chunk_with_rest(rest);
            let lst = eval(vm, arg);
            match lst {
                Value::List(List::Cons(lst)) => Value::List(lst.next),
                Value::List(List::Nil) => panic!("{arg:?} is a empty list."),
                _ => panic!("{arg:?} is not a list."),
            }
        }
        // 式展開を行わず文字列表現を返す
        "#" => {
            if is_block(rest) {
                Value::Str(extract_block_inner(rest).into())
            } else {
                panic!("# expects block. found: {rest:?}")
            }
        }
        "-" => sub(vm, rest),
        "+" => add(vm, rest),
        "*" => mul(vm, rest),
        "/" => div(vm, rest),
        "<" => {
            let tokens = extract_n_chunks(2, rest);
            let v1 = eval(vm, tokens.get(0).unwrap());
            let v2 = eval(vm, tokens.get(1).unwrap());
            match (v1, v2) {
                (Value::Num(i), Value::Num(j)) => Value::Bool(i < j),
                _ => panic!("< expects number."),
            }
        }
        "==" => {
            let tokens = extract_n_chunks(2, rest);
            let v1 = eval(vm, tokens.get(0).unwrap());
            let v2 = eval(vm, tokens.get(1).unwrap());
            Value::Bool(v1 == v2)
        }
        "!=" => {
            let tokens = extract_n_chunks(2, rest);
            let v1 = eval(vm, tokens.get(0).unwrap());
            let v2 = eval(vm, tokens.get(1).unwrap());
            Value::Bool(v1 != v2)
        }
        "if" => {
            let chunks = extract_n_chunks(3, rest);
            let bool_result = eval_block(vm, chunks.get(0).unwrap());
            match bool_result {
                Value::Bool(bool) => {
                    if bool {
                        eval(vm, chunks.get(1).unwrap())
                    } else {
                        eval(vm, chunks.get(2).unwrap())
                    }
                }
                _ => panic!("{bool_result:?} is not a boolean value."),
            }
        }
        "let" => {
            let chunks = extract_n_chunks(2, rest);
            let value = eval(vm, chunks.get(1).unwrap());
            let key = *chunks.get(0).unwrap();
            assign(vm, key, Vars::Var(value));
            Value::Nil
        }
        "def" => {
            let chunks = extract_n_chunks(3, rest);
            let key = *chunks.get(0).unwrap();
            let args = *chunks.get(1).unwrap();
            let body = *chunks.get(2).unwrap();
            let function = Function {
                args_block: args.into(),
                body_block: body.into(),
            };
            assign(vm, key, Vars::Function(function));
            Value::Nil
        }
        "print" => {
            let (arg, _) = extract_next_chunk_with_rest(rest);
            let value = eval(vm, arg);
            print(&value);
            value
        }
        _ => {
            let symbol = operator.to_string();
            let value = vm.vars[0].get(&symbol).cloned();
            if let Some(var) = value {
                match var {
                    Vars::Var(v) => panic!("{v:?} can't be called"),
                    Vars::Function(Function {
                        args_block,
                        body_block,
                    }) => {
                        // 関数の各変数に値をバインドして変数として宣言
                        let variables = extract_block_inner(&args_block)
                            .split_whitespace()
                            .into_iter()
                            .collect::<Vec<&str>>();
                        let args = extract_n_chunks(variables.len(), rest);

                        // 関数の評価時に新スコープ分の変数用HashMapを追加
                        let scoped_vars_container = variables
                            .iter()
                            .map(|&key| key.into())
                            .zip(args.iter().map(|arg| Vars::Var(eval(vm, arg))))
                            .collect::<HashMap<String, Vars>>();
                        let is_empty = scoped_vars_container.is_empty();
                        if !is_empty {
                            vm.vars.push(scoped_vars_container);
                        }
                        let result = eval_block(vm, &body_block);
                        if !is_empty {
                            vm.vars.pop();
                        }
                        return result;
                    }
                }
            }
            Value::Nil
        }
    }
}

fn assign(vm: &mut Vm, key: &str, var: Vars) {
    vm.vars
        .iter_mut()
        .rev()
        .nth(0)
        .and_then(|mp| mp.insert(key.into(), var));
}

fn skip_whitespace(s: &str) -> &str {
    let mut input = s;
    while let Some(_ch @ (' ' | '\n' | '\r')) = input.chars().next() {
        let mut chars = input.chars();
        chars.next();
        input = chars.as_str();
    }
    input
}

fn extract_n_chunks(n: usize, chunk: &str) -> Vec<&str> {
    let mut buff = Vec::new();
    let mut rest = chunk;
    for _ in 0..n {
        let (token, _rest) = extract_next_chunk_with_rest(rest);
        buff.push(token);
        rest = _rest;
    }
    buff
}

fn extract_next_chunk_with_rest(chunk: &str) -> (&str, &str) {
    let fst = extract_next_chunk(chunk);
    let ln = fst.len();
    let snd = (&chunk[ln..]).trim();
    (fst, snd)
}

fn eval(vm: &mut Vm, _value: &str) -> Value {
    let value = _value.trim();
    if is_block(value) {
        eval_block(vm, value)
    } else if let Some(i) = value.parse::<i32>().ok() {
        Value::Num(i)
    } else {
        let symbol = value;
        for vars in vm.vars.iter().rev() {
            if let Some(var) = vars.get(symbol) {
                match var {
                    Vars::Var(v) => return v.clone(),
                    _ => panic!("variable {var:?} is a function."),
                }
            }
        }
        panic!("variable {value:?} hasn't yet declared.");
    }
}

fn is_block(input: &str) -> bool {
    input.trim().starts_with('(')
}

// 対応する"(" と ")"の間の文字列を抜き出す
fn extract_block<'a>(s: &'a str) -> &'a str {
    let mut i = 0;
    let mut paren_cnt = 0;
    let chars = s.chars().into_iter().collect::<Vec<char>>();

    while let Some(&char) = chars.get(i) {
        if char == '(' {
            paren_cnt += 1;
        } else if char == ')' {
            paren_cnt -= 1;
        }
        i += 1;
        if paren_cnt == 0 {
            break;
        }
    }
    &s[0..i]
}

// 空白・改行までの文字列を取り出す
fn extract_token(input: &str) -> &str {
    let mut i = 0;
    let mut rest = input.chars();
    while let Some(_char) = rest.next() {
        if _char == ' ' || _char == '\n' {
            break;
        }
        i += 1;
    }
    &input[0..i]
}

fn extract_next_chunk(input: &str) -> &str {
    if let Some('(') = input.chars().next() {
        extract_block(input)
    } else {
        extract_token(input)
    }
}

// "(" .. ")"を取り出す
fn extract_block_inner<'a>(block: &'a str) -> &'a str {
    let ln = block.len();
    &block[1..ln - 1]
}

fn parse(vm: &mut Vm) {
    let input = skip_whitespace(vm.input);
    // 基底部
    if input.is_empty() {
        return;
    }
    let block = extract_block(input);
    let _result = eval_block(vm, block);
    vm.input = &vm.input.get((block.len() + 1)..).unwrap_or("").trim();
    parse(vm);
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let filename = args.get(1).expect("No filename specified.");
    let mut f = std::fs::File::open(filename).expect("File not found.");
    let mut input = String::new();
    f.read_to_string(&mut input)
        .expect("Something went wrong while reading the file.");
    let mut vm = Vm::new(&input);
    parse(&mut vm);
}
