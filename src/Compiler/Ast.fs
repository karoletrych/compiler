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

and IdentifierRef = 
  { Identifier : string }

and Statement = 
  | FunctionCallStatement of FunctionCallExpression
  | CompoundStatement of CompoundStatement
  | IfStatement of Expression * Statement * Statement option
  | WhileStatement of Expression * Statement
  | ReturnStatement of Expression option
  | VariableDeclarationStatement of VariableDeclaration
  | BreakStatement

and VariableDeclaration = 
  | VariableDeclaration of Identifier * TypeSpec option * Expression option
  | ValueDeclaration of Identifier * TypeSpec option * Expression

and CompoundStatement = Statement list

and Expression = 
  | ScalarAssignmentExpression of IdentifierRef * Expression
  | BinaryExpression of Expression * BinaryOperator * Expression
  | UnaryExpression of UnaryOperator * Expression
  | IdentifierExpression of IdentifierRef
  | ArrayIdentifierExpression of IdentifierRef * Expression
  | FunctionCallExpression of FunctionCallExpression
  | LiteralExpression of Literal
  | ArrayAllocationExpression of TypeSpec * Expression

and FunctionCallExpression = Identifier * Arguments

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

and Arguments = Expression list

and UnaryOperator = 
  | LogicalNegate
  | Negate
  | Identity

and Literal = 
  | BoolLiteral of bool
  | IntLiteral of int
  | FloatLiteral of float
  | StringLiteral of string
