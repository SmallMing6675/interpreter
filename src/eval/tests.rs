#[cfg(test)]
mod tests {

    use crate::eval::eval::*;
    use crate::parse::ast::*;

    #[test]
    fn test_eval_literal_int() {
        let mut env = Environment::new();
        let result = eval(&ASTNode::Literal(Literal::Int(42)), &mut env).unwrap();
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_eval_literal_float() {
        let mut env = Environment::new();
        let result = eval(&ASTNode::Literal(Literal::Float(3.14)), &mut env).unwrap();
        assert_eq!(result, Value::Float(3.14));
    }

    #[test]
    fn test_eval_literal_string() {
        let mut env = Environment::new();
        let result = eval(
            &ASTNode::Literal(Literal::Str("hello".to_string())),
            &mut env,
        )
        .unwrap();
        assert_eq!(result, Value::Str("hello".to_string()));
    }

    #[test]
    fn test_eval_literal_bool_true() {
        let mut env = Environment::new();
        let result = eval(&ASTNode::Literal(Literal::True), &mut env).unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn test_eval_literal_bool_false() {
        let mut env = Environment::new();
        let result = eval(&ASTNode::Literal(Literal::False), &mut env).unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn test_eval_variable_usage_existing() {
        let mut env = Environment::new();
        env.set("x".to_string(), Value::Int(10));
        let result = eval(&ASTNode::VariableUsage("x".to_string(), None), &mut env).unwrap();
        assert_eq!(result, Value::Int(10));
    }

    #[test]
    fn test_eval_variable_usage_non_existing() {
        let mut env = Environment::new();
        let result = eval(&ASTNode::VariableUsage("y".to_string(), None), &mut env);
        assert!(result.is_err());
    }

    #[test]
    fn test_environment_set_get() {
        let mut env = Environment::new();
        env.set("x".to_string(), Value::Int(10));
        let result = env.get("x").unwrap();
        assert_eq!(result, &Value::Int(10));
    }

    #[test]
    fn test_environment_set_overwrite() {
        let mut env = Environment::new();
        env.set("x".to_string(), Value::Int(10));
        env.set("x".to_string(), Value::Float(3.14));
        let result = env.get("x").unwrap();
        assert_eq!(result, &Value::Float(3.14));
    }

    #[test]
    fn test_environment_delete_existing() {
        let mut env = Environment::new();
        env.set("x".to_string(), Value::Int(10));
        let result = env.delete("x");
        assert_eq!(result, Some(Value::Int(10)));
        assert!(env.get("x").is_none());
    }

    #[test]
    fn test_environment_delete_non_existing() {
        let mut env = Environment::new();
        let result = env.delete("y");
        assert_eq!(result, None);
    }

    // Add more tests to cover other scenarios...
}
