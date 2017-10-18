module Compiler.Ast

type Program = Declaration list

and Declaration = 
  | FunctionDeclaration of Function
  | ClassDeclaration of Class

and DottedTypeName = Identifier list * CustomType

and Class = {
  Name : Identifier;
  GenericTypeParameters : GenericTypeParameter list;
  ImplementedInterfaces : DottedTypeName list;
  Properties : PropertyDeclaration list;
  Constructor : Constructor option;
  FunctionDeclarations : Function list;
  BaseClass : DottedTypeName option
}
and PropertyDeclaration =
  Identifier * TypeSpec * Expression option 

and Constructor = {
    Parameters : Parameter list;
    BaseClassConstructorCall : Arguments;
    Statements : Statement list;
}

and GenericTypeParameter = GenericTypeParameter of Identifier

and Function = { 
  Name : Identifier;
  GenericParameters : GenericTypeParameter list;
  Parameters : Parameter list;
  ReturnType : TypeSpec option;
  Body : CompoundStatement 
}

and TypeSpec = 
  | Bool
  | Char
  | Int
  | Float
  | Double
  | String
  | Void
  | CustomTypeSpec of DottedTypeName
  override ts.ToString() =
   let rec serializeTypeSpec ts = 
     match ts with 
     | CustomTypeSpec (ns, CustomType(id, generics)) ->
      (ns |> List.map (fun id -> id  + ".") |> String.concat "") 
      + id
      + if List.isEmpty generics
        then ""
        else "`" + (List.length generics).ToString() + "[" + (generics |> List.map serializeTypeSpec |> String.concat ",") + "]"
     | Bool -> "System.Bool"
     | Char -> "System.Char"
     | Int -> "System.Int32"
     | Float -> "System.Single"
     | Double -> "System.Double"
     | String -> "System.String"
     | Void -> failwith "generic can't be void"
   serializeTypeSpec ts

and CustomType = CustomType of Identifier * TypeSpec list

and Identifier = string

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
  | ListInitializerExpression of Expression list

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
