module Compiler.Ast

type Declaration =
| FunctionDeclaration of Function
| ClassDeclaration of Class

and Class = {
    Name : string;
    GenericTypeParameters : GenericTypeParameter list;
    BaseClass : TypeSpec option
    ImplementedInterfaces : TypeSpec list;
    Properties : PropertyDeclaration list;
    Constructor : Constructor option;
    FunctionDeclarations : Function list;
}


and PropertyDeclaration =
  string * TypeSpec * Expression option

and Constructor = {
    Parameters : Parameter list;
    BaseClassConstructorCall : Arguments;
    Statements : Statement list;
}

and GenericTypeParameter = GenericTypeParameter of string // TODO: it should be TypeSpec example: A<IEnumerable<T>> 

and Function = {
  Name : string;
  GenericParameters : GenericTypeParameter list;
  Parameters : Parameter list;
  ReturnType : TypeSpec option;
  Body : Statement list
}



and TypeSpec =
| BuiltInTypeSpec of BuiltInTypeSpec
| CustomTypeSpec of DottedTypeName

and BuiltInTypeSpec = 
| Bool
| Char
| Int
| Float
| Double
| String
| Void
| Object

and DottedTypeName = string list * CustomType

and CustomType = { 
    Name : string 
    GenericArgs : TypeSpec list
}


and Parameter = string * TypeSpec

and Statement =
| AssignmentStatement of Assignment
| BreakStatement
| CompositeStatement of Statement list
| FunctionCallStatement of FunctionCall
| IfStatement of Expression * Statement * Statement option
| MemberFunctionCallStatement of MemberFunctionCall
| ReturnStatement of Expression option
| StaticFunctionCallStatement of TypeSpec * FunctionCall
| VariableDeclaration of VariableDeclaration
| ValueDeclaration of ValueDeclaration
| WhileStatement of Expression * Statement


and ValueDeclaration =
  string * TypeSpec option * Expression
and VariableDeclaration =
| DeclarationWithInitialization of string * Expression
| DeclarationWithType of string * TypeSpec
| FullDeclaration of string * TypeSpec * Expression



and IExpression = interface end
//TODO: http://theburningmonk.com/2012/03/f-extending-discriminated-unions-using-marker-interfaces/

and Expression =
| AssignmentExpression of Assignment
| BinaryExpression of Expression * BinaryOperator * Expression
| ExpressionWithInferredType of Expression * string option
| FunctionCallExpression of FunctionCall
| IdentifierExpression of string
| ListInitializerExpression of Expression list
| LiteralExpression of Literal
| MemberExpression of MemberFunctionCall
| NewExpression of TypeSpec * Arguments
| StaticMemberExpression of TypeSpec * FunctionCall
| UnaryExpression of UnaryOperator * Expression

and MemberFunctionCall = MemberFunctionCall of Expression * Expression

and Assignment = Expression * Expression

and FunctionCall = {
     Name : string;
     GenericArguments : TypeSpec list;
     Arguments : Arguments 
     }

and Arguments = Expression list

and BinaryOperator =
| ConditionalOr
| ConditionalAnd
| Equal
| NotEqual
| LessEqual
| Less
| GreaterEqual
| Greater
| Plus
| Minus
| Multiplication
| Division
| Remainder

and UnaryOperator =
| LogicalNegate
| Negate

and Literal =
| BoolLiteral of bool
| IntLiteral of int
| FloatLiteral of float
| StringLiteral of string

type Module = {
        Classes : Class list
    } with
    static member (+) (m1 : Module, m2 : Module) =
          { Classes = m1.Classes @ m2.Classes}
    
    end
module Module =
    let create (moduleName : string) declarations =
        let functions = declarations |> List.choose ( fun m ->
                        match m with
                        | FunctionDeclaration f -> Some f
                        | _ -> None)
        let classes = declarations |> List.choose ( fun m ->
                        match m with
                        | ClassDeclaration c -> Some c
                        | _ -> None)
        let moduleFunctionsInAStaticClass = {
            GenericTypeParameters = []
            Name = moduleName;
            FunctionDeclarations = functions;
            BaseClass = None;
            ImplementedInterfaces = [];
            Properties = [];
            Constructor = None
            }
        let moduleClasses = classes |> List.map (fun c -> {c with Name = moduleName + "." + c.Name })
        let classes = moduleFunctionsInAStaticClass :: moduleClasses
        { Classes = classes }
    let createDefault declarations = create "DEFAULT" declarations 
    let identity = {Classes = []}
    let plus m1 m2 = {Classes = m1.Classes @ m2.Classes}

