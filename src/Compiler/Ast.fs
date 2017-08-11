module Compiler.Ast

type Program = Declaration list

and Declaration = 
  | FunctionDeclaration of FunctionDeclaration
  | TypeDeclaration of string * Declaration list

and IsReadonly = bool

and FunctionDeclaration = Identifier * Parameter list * TypeSpec option * CompoundStatement

and TypeSpec = 
  | Bool
  | Char
  | Int
  | Float
  | Double
  | String
  | Void
  | UserDefinedType

and UserDefinedType = 
  | NonGenericUserDefinedType

and NonGenericUserDefinedType = string

and Identifier = string

and Parameter = Identifier * TypeSpec option

and Statement = 
  | FunctionCallStatement of FunctionCall
  | AssignmentStatement of Assignment
  | CompoundStatement of CompoundStatement
  | IfStatement of Expression * Statement * Statement option
  | WhileStatement of Expression * Statement
  | ReturnStatement of Expression option
  | VariableDeclaration of Identifier * TypeSpec option * Expression option
  | ValueDeclaration of Identifier * TypeSpec option * Expression
  | BreakStatement


and CompoundStatement = Statement list

and Expression = 
  | AssignmentExpression of Assignment
  | BinaryExpression of Expression * BinaryOperator * Expression
  | UnaryExpression of UnaryOperator * Expression
  | IdentifierExpression of Identifier
  | FunctionCallExpression of FunctionCall
  | LiteralExpression of Literal

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
  | Identity

and Literal = 
  | BoolLiteral of bool
  | IntLiteral of int
  | FloatLiteral of float
  | StringLiteral of string
