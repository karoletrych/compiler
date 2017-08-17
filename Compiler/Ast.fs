module Compiler.Ast

type Program = Declaration list

and Declaration = 
  | FunctionDeclaration of FunctionDeclaration
  | ClassDeclaration of ClassDeclaration

and ClassDeclaration = {
  Type : NonGenericTypeSpec;
  GenericTypeParameters : GenericTypeParameter list;
  BaseTypes : TypeSpec list;
  ValueDeclarations : ValueDeclaration list;
  FieldsDeclarations : VariableDeclaration list;
  Constructor : Constructor option;
  FunctionDeclarations : FunctionDeclaration list;
}

and Constructor = {
    Parameters : Parameter list;
    BaseClassConstructorCall : Arguments;
    Statements : Statement list;
}

and GenericTypeParameter = GenericTypeParameter of Identifier

and FunctionDeclaration = Identifier * Parameter list * TypeSpec option * CompoundStatement

and TypeSpec = 
  | Bool
  | Char
  | Int
  | Float
  | Double
  | String
  | Void
  | CustomType of CustomTypeSpec

and CustomTypeSpec = 
| NonGenericCustomTypeSpec of NonGenericTypeSpec
| GenericCustomTypeSpec of NonGenericTypeSpec * TypeSpec list

and NonGenericTypeSpec = NonGenericTypeSpec of Identifier

and Identifier = Identifier of string

and Parameter = Identifier * TypeSpec

and Statement = 
  | FunctionCallStatement of FunctionCall
  | AssignmentStatement of Assignment
  | CompoundStatement of CompoundStatement
  | IfStatement of Expression * Statement * Statement option
  | WhileStatement of Expression * Statement
  | ReturnStatement of Expression option
  | VariableDeclaration of VariableDeclaration
  | ValueDeclaration of ValueDeclaration
  | BreakStatement

and ValueDeclaration =
  Identifier * TypeSpec option * Expression
and VariableDeclaration =
  | DeclarationWithInitialization of Identifier * Expression
  | DeclarationWithType of Identifier * TypeSpec
  | FullDeclaration of Identifier * TypeSpec * Expression

and CompoundStatement = Statement list

and Expression = 
  | AssignmentExpression of Assignment
  | BinaryExpression of Expression * BinaryOperator * Expression
  | UnaryExpression of UnaryOperator * Expression
  | IdentifierExpression of Identifier
  | FunctionCallExpression of FunctionCall
  | LiteralExpression of Literal
  | NewExpression of CustomTypeSpec * Arguments

and Assignment = Identifier * Expression

and FunctionCall = Identifier * Arguments

and Arguments = Expression list

and BinaryOperator = 
  | ConditionalOr
  | Equal
  | NotEqual
  | LessEqual
  | Less
  | GreaterEqual
  | Greater
  | ConditionalAnd
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulus

and UnaryOperator = 
  | LogicalNegate
  | Negate

and Literal = 
  | BoolLiteral of bool
  | IntLiteral of int
  | FloatLiteral of float
  | StringLiteral of string
