#[derive(Debug, PartialEq)]
pub enum ASTNode {
    VariableDeclaration(Box<VariableDeclarationAST>),
    FunctionDefinition(Box<FunctionDefinitionAST>),
    FunctionCall(Box<FunctionCallAST>),
    TypeDeclaration(Box<TypeDeclarationAST>),
    Literal(Literal),
    VariableUsage(String, Option<Type>),
    BinaryOperation(Box<ASTNode>, BinaryOperator, Box<ASTNode>),
    VariableDeletion(String),
    InlineFunction(Vec<String>, Box<ASTNode>),
    Match(Box<ASTNode>, Vec<ASTMatchArm>),
}

#[derive(Debug, PartialEq)]
pub struct VariableDeclarationAST {
    pub identifier: String,
    pub expression: Box<ASTNode>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionDefinitionAST {
    pub identifier: String,
    pub parameters: Vec<Parameter>,
    pub body: Box<ASTNode>,
}

#[derive(Debug, PartialEq)]
pub struct Parameter {
    pub identifier: String,
    pub type_: Type,
}

#[derive(Debug, PartialEq)]
pub struct FunctionCallAST {
    pub identifier: String,
    pub arguments: Vec<ASTNode>,
}

#[derive(Debug, PartialEq)]
pub struct TypeDeclarationAST {
    pub identifier: String,
    pub type_: Type,
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

#[derive(Debug, PartialEq)]
pub enum ASTMatchArm {
    MatchCondition(Option<ASTNode>, ASTNode),
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Str(String),
    True,
    False,
    List(Vec<ASTNode>),
}
