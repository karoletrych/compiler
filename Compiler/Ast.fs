module Compiler.Ast

type Program = Declaration list

and Declaration = 
  | FunctionDeclaration of FunctionDeclaration
  | ClassDeclaration of ClassDeclaration

and ClassDeclaration = {
  Type : SimpleTypeSpec;
  GenericTypeParameters : GenericTypeParameter list;
  BaseTypes : (Identifier list * CustomType) list;
  ValueDeclarations : ValueFieldDeclaration list;
  FieldDeclarations : VariableDeclaration list;
  Constructor : Constructor option;
  FunctionDeclarations : FunctionDeclaration list;
}

and Constructor = {
    Parameters : Parameter list;
    BaseClassConstructorCall : Arguments;
    Statements : Statement list;
}

and GenericTypeParameter = GenericTypeParameter of Identifier

and FunctionDeclaration = Identifier * GenericTypeParameter list * Parameter list * TypeSpec option * CompoundStatement

and TypeSpec = 
  | Bool
  | Char
  | Int
  | Float
  | Double
  | String
  | Void
  | CustomTypeSpec of Identifier list * CustomType

and CustomType = 
| SimpleCustomTypeSpec of SimpleTypeSpec
| GenericCustomTypeSpec of SimpleTypeSpec * TypeSpec list

and SimpleTypeSpec = SimpleTypeSpec of Identifier

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
  | MemberFunctionCallStatement of MemberFunctionCall 
  | StaticFunctionCallStatement of TypeSpec * FunctionCall


and ValueDeclaration =
  Identifier * TypeSpec option * Expression
and ValueFieldDeclaration =
  Identifier * TypeSpec option * Expression option // can be assigned in constructor
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
  | NewExpression of TypeSpec * Arguments
  | MemberExpression of MemberFunctionCall
  | StaticMemberExpression of TypeSpec * FunctionCall

and MemberFunctionCall = MemberFunctionCall of Expression * Expression

and Assignment = Expression * Expression

and FunctionCall = FunctionCall of Identifier * TypeSpec list * Arguments

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
