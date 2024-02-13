use crate::eval::eval::Value;
use crate::parse::ast::*;
use std::collections::HashMap;

lazy_static! {
    pub static ref BUILTIN_FUNCTIONS: HashMap<String, Value> = {
        let mut builtins = HashMap::new();
        builtins.insert(
            "print".to_string(),
            Value::BuiltinFunction(
                "print".to_string(),
                Parameter {
                    identifier: "input".to_string(),
                    type_: None,
                },
                print_,
            ),
        );

        builtins
    };
}
type BuiltInFunction = fn(Value) -> Value;

fn print_(input: Value) -> Value {
    match input {
        Value::Int(val) => println!("{}", val),
        Value::Float(val) => println!("{}", val),
        Value::Str(val) => println!("{}", val),
        Value::Bool(val) => println!("{}", val),
        Value::List(vals) => {
            for val in vals {
                print_(val); // Recursively call print_ for each element in the list
            }
        }
        Value::Empty => println!("{{Empty}}"),
        Value::Function(param, _, _) => println!("Function({})", param.identifier),
        Value::BuiltinFunction(name, _, _) => println!("BuiltinFunction({})", name),
    }
    Value::Empty
}
