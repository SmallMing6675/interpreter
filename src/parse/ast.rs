#[derive(Debug, PartialEq, Clone)]
pub enum ASTNode {
    VariableDeclaration(String, Box<ASTNode>, Option<Type>),
    FunctionDefinition(Parameter, Box<ASTNode>),
    FunctionCall(Box<ASTNode>, Box<ASTNode>), //name, arg
    TypeDeclaration(String, Type),
    Literal(Literal),
    VariableUsage(String, Option<Type>),
    BinaryOperation(Box<ASTNode>, BinaryOperator, Box<ASTNode>),
    VariableDeletion(String),
    InlineFunction(Vec<Parameter>, Box<ASTNode>),
    If(Box<ASTNode>, Box<ASTNode>, Option<Box<ASTNode>>),
    Match(Box<ASTNode>, Vec<ASTMatchArm>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Parameter {
    pub identifier: String,
    pub type_: Option<Type>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Int,
    Float,
    Str,
    Bool,
    List(Box<Type>),
    Function(Box<Type>, Box<Type>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ASTMatchArm {
    MatchCondition(Option<ASTNode>, ASTNode),
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,

    And,
    Or,
    Not,
    Xor,
    Assign,

    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Str(String),
    True,
    False,
    List(Vec<ASTNode>),
}
