module Compiler.Ast

type Program = Declaration list

and Declaration =
  | StaticVariableDeclaration of VariableDeclaration
  | FunctionDeclaration of FunctionDeclaration
  | TypeDeclaration of string * Declaration list

and VariableDeclaration = 
    | ScalarVariableDeclaration of TypeSpec option * Identifier

and FunctionDeclaration = Identifier * Parameters * TypeSpec option * CompoundStatement

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

and Parameters = VariableDeclaration list

and IdentifierRef = { Identifier : string; }

and Statement =
  | ExpressionStatement of ExpressionStatement
  | CompoundStatement of CompoundStatement
  | IfStatement of IfStatement
  | WhileStatement of WhileStatement
  | ReturnStatement of Expression option
  | BreakStatement
  | VariableDeclaration

and ExpressionStatement = Expression

and LocalDeclarations = VariableDeclaration list


and CompoundStatement =  Statement list

and IfStatement = Expression * Statement * Statement option

and WhileStatement = Expression * Statement

and Expression =
  | ScalarAssignmentExpression of IdentifierRef * Expression
  | ArrayAssignmentExpression of IdentifierRef * Expression * Expression
  | BinaryExpression of Expression * BinaryOperator * Expression
  | UnaryExpression of UnaryOperator * Expression
  | IdentifierExpression of IdentifierRef
  | ArrayIdentifierExpression of IdentifierRef * Expression
  | FunctionCallExpression of Identifier * Arguments
  | ArraySizeExpression of IdentifierRef
  | LiteralExpression of Literal
  | ArrayAllocationExpression of TypeSpec * Expression

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
