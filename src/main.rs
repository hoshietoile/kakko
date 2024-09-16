use std::collections::HashMap;

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

struct Vm<'a> {
    input: &'a str,
    vars: HashMap<String, Vars>,
}

impl<'a> Vm<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            vars: HashMap::new(),
        }
    }
}

fn skip_whitespace(s: &str) -> &str {
    let mut input = s;
    while let Some(_ch @ (' ' | '\n')) = input.chars().next() {
        let mut chars = input.chars();
        chars.next();
        input = chars.as_str();
    }
    input
}

fn parse_args() {}

#[derive(Debug, Eq, PartialEq, Clone)]
enum Value {
    Num(i32),
    Bool(bool),
    Def,         // TODO:
    Sym(String), // TODO:
    Nil,
}

impl Value {
    fn as_num(&self) -> i32 {
        match self {
            Value::Num(i) => *i,
            _ => panic!("Parse Error. Value: {self:?}"),
        }
    }
}

macro_rules! impl_op {
    {$name:ident, $op:tt} => {
        fn $name(vm: &mut Vm, input: &str) -> Value {
            let (arg1, rest) = extract_next_chunk_with_rest(input);
            let (arg2, _rest) = extract_next_chunk_with_rest(rest);
            let v1 = eval(vm, arg1);
            let v2 = eval(vm, arg2);
            match (v1, v2) {
                (Value::Num(i), Value::Num(j)) => Value::Num(i $op j),
                _ => panic!("Parse error!"),
            }
        }
    }
}
impl_op!(add, +);
impl_op!(sub, -);
impl_op!(mul, *);
impl_op!(div, /);

fn eval_block(vm: &mut Vm, block: &str) -> Value {
    let inner = extract_block_inner(block);
    let operator = extract_value(inner);
    let op_len = operator.len();
    let rest = (&inner[op_len..]).trim();

    match operator {
        "-" => sub(vm, rest),
        "+" => add(vm, rest),
        "*" => mul(vm, rest),
        "/" => div(vm, rest),
        "<" => {
            let (arg1, rest) = extract_next_chunk_with_rest(rest);
            let (arg2, _rest) = extract_next_chunk_with_rest(rest);
            let v1 = eval(vm, arg1);
            let v2 = eval(vm, arg2);
            match (v1, v2) {
                (Value::Num(i), Value::Num(j)) => Value::Bool(i < j),
                _ => panic!("Parse error!"),
            }
        }
        "=" => {
            let (arg1, rest) = extract_next_chunk_with_rest(rest);
            let (arg2, _rest) = extract_next_chunk_with_rest(rest);
            let v1 = eval(vm, arg1);
            let v2 = eval(vm, arg2);
            Value::Bool(v1 == v2)
        }
        "if" => {
            let (arg1, rest) = extract_next_chunk_with_rest(rest);
            let (arg2, rest) = extract_next_chunk_with_rest(rest);
            let (arg3, _rest) = extract_next_chunk_with_rest(rest);
            let bool_result = eval_block(vm, arg1);
            match bool_result {
                Value::Bool(bool) => {
                    if bool {
                        eval(vm, arg2)
                    } else {
                        eval(vm, arg3)
                    }
                }
                _ => panic!("Not a bool value"),
            }
        }
        "let" => {
            let (arg1, rest) = extract_next_chunk_with_rest(rest);
            let (arg2, _) = extract_next_chunk_with_rest(rest);
            let var = eval(vm, arg2);
            vm.vars.insert(arg1.into(), Vars::Var(var));
            Value::Nil
        }
        "def" => {
            let (arg1, rest) = extract_next_chunk_with_rest(rest);
            let (arg2, rest) = extract_next_chunk_with_rest(rest);
            let (arg3, _rest) = extract_next_chunk_with_rest(rest);
            let function = Function {
                args_block: arg2.into(),
                body_block: arg3.into(),
            };
            vm.vars.insert(arg1.into(), Vars::Function(function));
            Value::Nil
        }
        _ => {
            let symbol = operator.to_string();
            let value = vm.vars.get(&symbol).cloned();
            if let Some(var) = value {
                match var {
                    Vars::Var(v) => panic!("{v:?} can't be called"),
                    Vars::Function(Function {
                        args_block,
                        body_block,
                    }) => {
                        // 関数の各変数に値をバインドして変数として宣言
                        let variables = extract_block_inner(&args_block).split_whitespace();
                        for variable in variables {
                            // TODO: restの宣言方法
                            let (arg, _) = extract_next_chunk_with_rest(rest);
                            let var = Vars::Var(eval(vm, arg));
                            vm.vars.insert(variable.into(), var);
                        }
                        return eval_block(vm, &body_block);
                    }
                }
            }
            Value::Nil
        }
    }
}

fn extract_next_chunk_with_rest(chunk: &str) -> (&str, &str) {
    let fst = extract_next_chunk(chunk);
    let ln = fst.len();
    let snd = (&chunk[ln..]).trim();
    (fst, snd)
}

fn eval(vm: &mut Vm, value: &str) -> Value {
    if is_block(value) {
        eval_block(vm, value)
    } else if let Some(i) = value.parse::<i32>().ok() {
        Value::Num(i)
    } else {
        let symbol = value;
        if let Some(var) = vm.vars.get(symbol) {
            match var {
                Vars::Var(v) => v.clone(),
                _ => panic!("Functino can't be called"),
            }
        } else {
            panic!("Var: {value:?} isn't yet declared");
        }
    }
}

fn is_block(input: &str) -> bool {
    input.trim().starts_with('(')
}

// "(" と ")"のブロックを取得する
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
fn extract_value<'a>(s: &'a str) -> &'a str {
    let mut i = 0;
    let _input = (&s[..]).trim();
    let mut rest = _input.chars();
    while let Some(_char) = rest.next() {
        if _char == ' ' || _char == '\n' {
            break;
        }
        i += 1;
    }
    &_input[0..i]
}

fn extract_next_chunk<'a>(s: &'a str) -> &'a str {
    let _input = (&s[..]).trim();

    if let Some('(') = _input.chars().next() {
        extract_block(_input)
    } else {
        extract_value(_input)
    }
}

// "(" .. ")"を取り出す
fn extract_block_inner<'a>(block: &'a str) -> &'a str {
    let ln = block.len();
    &block[1..ln - 1]
}

fn parse(vm: &mut Vm) {
    println!("####");
    println!("{:?}", vm.input);
    let input = skip_whitespace(vm.input);
    // 基底部
    if input.len() <= 0 {
        return;
    }
    let block = extract_block(input);
    let result = eval_block(vm, block);
    let ln = block.len();
    vm.input = &vm.input.get((ln + 1)..).unwrap_or("").trim();
    println!("{block:?}");
    println!("result: {result:?}");
    println!("{:?}", vm.vars);
    parse(vm);
}

fn main() {
    let input = "
(let x (+ 1501 1500))
(def double (v) (* v v))
(def isEven (v) (= 0 (- v (* 2 (/ v 2)))))
(if (isEven x) (if (= 10 1) (+ 0 1) 2) (if (= 10 1) (- 4 1) (double 4)))";
    let mut vm = Vm::new(input);
    parse(&mut vm);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse01() {
        let expr = "(+ 10 20)";
        assert_eq!(true, true);
    }

    fn test_parse02() {
        let expr = "(+ 10 (* 10 10))";
        assert_eq!(true, true);
    }

    fn test_parse03() {
        let expr = "(+ 10 (if (= 2 2) 20 10))";
        assert_eq!(true, true);
    }
}
