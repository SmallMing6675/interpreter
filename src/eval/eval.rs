use crate::parse::ast::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
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
            outer: Some(Box::new(outer.clone())),
        }
    }

    pub fn get(&self, key: &str) -> Option<Value> {
        match self.store.get(key) {
            Some(value) => Some(value.clone()),
            None => match &self.outer {
                Some(outer_env) => outer_env.get(key),
                None => None,
            },
        }
    }

    pub fn set(&mut self, key: String, value: Value) {
        self.store.insert(key, value);
    }

    pub fn delete(&mut self, key: String) -> Option<Value> {
        self.store.remove(&key)
    }
}
#[derive(Clone, Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    List(Vec<Value>),
    Function(String, Vec<Parameter>, ASTNode),
    Empty, // Used for operations that does not have a value (Not null)
}

impl Value {
    pub fn cast(self, type_: Type) -> Self {
        match (self, type_) {
            (Value::Int(val), Type::Float) => Value::Float(val as f64),
            (Value::Float(val), Type::Int) => Value::Int(val as i64),
            (Value::Int(val), Type::Bool) => Value::Bool(val != 0),
            (Value::Float(val), Type::Bool) => Value::Bool(val != 0.0),
            // Add more conversions as needed
            _ => todo!(), // Handle other conversions or return an error
        }
    }

    pub fn get_type(&self) -> Result<Type, EvalError> {
        Ok(match self {
            Value::Int(_) => Type::Int,
            Value::Float(_) => Type::Float,
            Value::Bool(_) => Type::Bool,
            Value::Str(_) => Type::Str,
            Value::List(values) => Type::List(Box::new(values[0].get_type()?)),
            _ => return Err(EvalError::InvalidType),
        })
    }

    pub fn truthy(&self) -> bool {
        match self {
            Value::Int(int) => *int != 0,
            Value::Float(float) => *float != 0.0,
            Value::Str(str) => str.len() != 0,
            Value::List(list) => list.len() != 0,
            Value::Bool(bool) => *bool,
            _ => false,
        }
    }
}

pub fn eval(node: &ASTNode, env: &mut Environment) -> Result<Value, EvalError> {
    match node {
        ASTNode::Literal(literal) => evaluate_literal(literal, env),
        ASTNode::VariableUsage(name, _) => lookup_variable(name, env),
        ASTNode::BinaryOperation(left, op, right) => {
            let left_value = eval(left, env)?;
            let right_value = eval(right, env)?;
            evaluate_binary_operation(op, left_value, right_value)
        }

        ASTNode::VariableDeclaration(name, value, type_) => {
            if env.get(name).is_some() {
                return Err(EvalError::AlreadyDefined(name.to_string()));
            }
            let mut value = eval(value, env)?;
            value = match type_ {
                Some(type_) if value.get_type()? != *type_ => value.clone().cast(type_.clone()),
                _ => value,
            };

            env.set(name.to_string(), value.clone());
            Ok(value)
        }

        ASTNode::VariableDeletion(name) => match env.delete(name.to_string()) {
            Some(_) => Ok(Value::Empty),
            None => Err(EvalError::VariableNotFound(name.to_string())),
        },

        ASTNode::If(condition, then, else_) => evaluate_if(condition, then, else_, env),

        ASTNode::FunctionDefinition(name, parameters, body) => {
            eval_function(name.to_string(), parameters, body, env)
        }

        _ => unimplemented!("Evaluation not implemented for this ASTNode variant"),
    }
}

fn eval_function(
    name: String,
    parameters: &Vec<Parameter>,
    body: &ASTNode,
    env: &mut Environment,
) -> Result<Value, EvalError> {
    let func_value = Value::Function(name.clone(), parameters.clone(), body.clone());
    env.set(name, func_value.clone());

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

fn lookup_variable(name: &str, env: &Environment) -> Result<Value, EvalError> {
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
}
