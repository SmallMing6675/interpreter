use crate::eval::builtins::BUILTIN_FUNCTIONS;
use crate::parse::ast::*;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    store: HashMap<String, Value>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: &Environment) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(Box::new(outer.clone())), // Clone the reference, not the Environment
        }
    }

    pub fn get(&self, key: &str) -> Option<&Value> {
        match self.store.get(key) {
            Some(value) => Some(value),
            None => self.outer.as_ref().and_then(|outer_env| outer_env.get(key)),
        }
    }

    pub fn set(&mut self, key: String, value: Value) -> &mut Self {
        self.store.insert(key, value);
        self
    }

    pub fn delete(&mut self, key: &str) -> Option<Value> {
        self.store.remove(key)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    List(Vec<Value>),
    Function(Parameter, ASTNode, Environment),
    BuiltinFunction(String, Parameter, fn(Value) -> Value),
    Empty, // Used for operations that does not have a value (Not null)
}

impl Value {
    pub fn cast(&self, type_: &Type) -> Result<Self, EvalError> {
        match (self, type_) {
            (Value::Int(val), Type::Float) => Ok(Value::Float(*val as f64)),
            (Value::Float(val), Type::Int) => Ok(Value::Int(*val as i64)),
            (Value::Int(val), Type::Bool) => Ok(Value::Bool(*val != 0)),
            (Value::Float(val), Type::Bool) => Ok(Value::Bool(*val != 0.0)),
            (Value::Bool(val), Type::Int) => Ok(Value::Int(*val as i64)),
            (Value::Bool(val), Type::Float) => Ok(Value::Float(*val as i64 as f64)),
            _ => Err(EvalError::CannotCast),
        }
    }

    pub fn get_type(&self) -> Result<Type, EvalError> {
        match self {
            Value::Int(_) => Ok(Type::Int),
            Value::Float(_) => Ok(Type::Float),
            Value::Bool(_) => Ok(Type::Bool),
            Value::Str(_) => Ok(Type::Str),
            Value::List(values) => Ok(Type::List(Box::new(values[0].get_type()?))),
            _ => Err(EvalError::InvalidType),
        }
    }

    pub fn truthy(&self) -> bool {
        match self {
            Value::Int(int) => *int != 0,
            Value::Float(float) => *float != 0.0,
            Value::Str(str) => !str.is_empty(),
            Value::List(list) => !list.is_empty(),
            Value::Bool(bool) => *bool,
            _ => false,
        }
    }
}

pub fn eval(node: &ASTNode, env: &mut Environment) -> Result<Value, EvalError> {
    match node {
        ASTNode::Literal(literal) => evaluate_literal(literal, env),
        ASTNode::VariableUsage(name, _) => lookup_variable(name, env).cloned(),
        ASTNode::BinaryOperation(left, op, right) => {
            let left_value = eval(left, env)?;
            let right_value = eval(right, env)?;
            evaluate_binary_operation(op, left_value, right_value)
        }

        ASTNode::VariableDeclaration(name, value, type_) => {
            if env.get(name).is_some() {
                return Err(EvalError::AlreadyDefined(name.to_string()));
            }
            let value = eval(value, env)?;
            let value = if let Some(type_) = type_ {
                if value.get_type()? != *type_ {
                    value.cast(type_)?
                } else {
                    value
                }
            } else {
                value
            };

            env.set(name.to_string(), value.clone());
            Ok(value)
        }

        ASTNode::VariableDeletion(name) => match env.delete(name) {
            Some(_) => Ok(Value::Empty),
            None => Err(EvalError::VariableNotFound(name.to_string())),
        },

        ASTNode::If(condition, then, else_) => evaluate_if(condition, then, else_, env),

        ASTNode::FunctionDefinition(param, body) => eval_function_def(param.clone(), body, env),

        ASTNode::InlineFunction(params, body) => {
            let param = params[0].clone();
            let func_body = if params.len() == 1 {
                body.clone()
            } else {
                Box::new(parse_inline_function(
                    params[1..].to_vec(),
                    body.clone(),
                    env,
                )?)
            };
            Ok(Value::Function(param, *func_body, env.clone()))
        }

        ASTNode::FunctionCall(function_name, arg) => {
            let func = eval(function_name, env)?;
            match func {
                Value::Function(param, body, mut func_env) => {
                    let arg_value = eval(arg, &mut func_env)?;
                    func_env.set(param.identifier.clone(), arg_value);
                    eval(&body, &mut func_env)
                }

                Value::BuiltinFunction(_, _, func) => Ok(func(eval(arg, env)?)),
                _ => Err(EvalError::NotAFunction),
            }
        }

        _ => unimplemented!("Evaluation not implemented for this ASTNode variant"),
    }
}

// fn a b = a + b
// |-> function(a) -> function(b) -> a + b
fn parse_inline_function(
    params: Vec<Parameter>,
    body: Box<ASTNode>,
    env: &mut Environment,
) -> Result<ASTNode, EvalError> {
    if params.len() == 1 {
        Ok(ASTNode::FunctionDefinition(params[0].clone(), body))
    } else {
        Ok(ASTNode::FunctionDefinition(
            params[0].clone(),
            Box::new(parse_inline_function(params[1..].to_vec(), body, env)?),
        ))
    }
}

fn eval_function_def(
    parameter: Parameter,
    body: &ASTNode,
    env: &mut Environment,
) -> Result<Value, EvalError> {
    let func_value = Value::Function(parameter.clone(), body.clone(), env.clone());

    Ok(func_value)
}
fn evaluate_if(
    condition: &ASTNode,
    then: &ASTNode,
    else_: &Option<Box<ASTNode>>,
    env: &mut Environment,
) -> Result<Value, EvalError> {
    if eval(condition, env)?.truthy() {
        return eval(then, env);
    } else {
        match else_ {
            Some(else_) => eval(else_, env),
            None => return Ok(Value::Empty),
        }
    }
}
fn evaluate_literal(literal: &Literal, env: &mut Environment) -> Result<Value, EvalError> {
    match literal {
        Literal::Int(value) => Ok(Value::Int(*value)),
        Literal::Float(value) => Ok(Value::Float(*value)),
        Literal::Str(value) => Ok(Value::Str(value.clone())),
        Literal::True => Ok(Value::Bool(true)),
        Literal::False => Ok(Value::Bool(false)),
        Literal::List(list) => Ok(Value::List(
            list.iter()
                .map(|element| eval(element, env))
                .into_iter()
                .collect::<Result<Vec<Value>, _>>()?,
        )),
    }
}

fn lookup_variable<'a>(name: &str, env: &'a Environment) -> Result<&'a Value, EvalError> {
    match BUILTIN_FUNCTIONS.get(name) {
        Some(func) => return Ok(func),
        None => (),
    };
    env.get(name)
        .ok_or_else(|| EvalError::VariableNotFound(name.to_string()))
}

fn evaluate_binary_operation(
    op: &BinaryOperator,
    left_value: Value,
    right_value: Value,
) -> Result<Value, EvalError> {
    match (left_value.clone(), right_value.clone()) {
        (Value::Int(left), Value::Int(right)) => match op {
            BinaryOperator::Plus => Ok(Value::Int(left + right)),
            BinaryOperator::Minus => Ok(Value::Int(left - right)),
            BinaryOperator::Multiply => Ok(Value::Int(left * right)),
            BinaryOperator::Divide => {
                if right == 0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(Value::Int(left / right))
                }
            }
            BinaryOperator::Modulo => {
                if right == 0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(Value::Int(left % right))
                }
            }
            BinaryOperator::Equal => Ok(Value::Bool(left == right)),
            BinaryOperator::NotEqual => Ok(Value::Bool(left != right)),
            BinaryOperator::LessThan => Ok(Value::Bool(left < right)),
            BinaryOperator::LessThanOrEqual => Ok(Value::Bool(left <= right)),
            BinaryOperator::GreaterThan => Ok(Value::Bool(left > right)),
            BinaryOperator::GreaterThanOrEqual => Ok(Value::Bool(left >= right)),
            _ => return Err(EvalError::InvalidTypeCombination(left_value, right_value)),
        },
        (Value::Float(left), Value::Float(right)) => match op {
            BinaryOperator::Plus => Ok(Value::Float(left + right)),
            BinaryOperator::Minus => Ok(Value::Float(left - right)),
            BinaryOperator::Multiply => Ok(Value::Float(left * right)),
            BinaryOperator::Divide => {
                if right == 0.0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(Value::Float(left / right))
                }
            }
            BinaryOperator::Modulo => {
                if right == 0.0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(Value::Float(left % right))
                }
            }
            BinaryOperator::Equal => Ok(Value::Bool(left == right)),
            BinaryOperator::NotEqual => Ok(Value::Bool(left != right)),
            BinaryOperator::LessThan => Ok(Value::Bool(left < right)),
            BinaryOperator::LessThanOrEqual => Ok(Value::Bool(left <= right)),
            BinaryOperator::GreaterThan => Ok(Value::Bool(left > right)),
            BinaryOperator::GreaterThanOrEqual => Ok(Value::Bool(left >= right)),
            _ => return Err(EvalError::InvalidTypeCombination(left_value, right_value)),
        },
        (Value::Bool(left), Value::Bool(right)) => match op {
            BinaryOperator::And => Ok(Value::Bool(left && right)),
            BinaryOperator::Or => Ok(Value::Bool(left || right)),
            BinaryOperator::Xor => Ok(Value::Bool(left ^ right)),
            BinaryOperator::Equal => Ok(Value::Bool(left == right)),
            BinaryOperator::NotEqual => Ok(Value::Bool(left != right)),
            _ => return Err(EvalError::InvalidTypeCombination(left_value, right_value)),
        },
        _ => Err(EvalError::InvalidTypeCombination(left_value, right_value)),
    }
}
#[derive(Debug)]
pub enum EvalError {
    NotAFunction,
    VariableNotFound(String),
    InvalidTypeCombination(Value, Value),
    InvalidType,
    AlreadyDefined(String),
    DivisionByZero,
    CannotCast,
}
