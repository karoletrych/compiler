module Compiler.Ast

type Declaration =
| FunctionDeclaration of Function
| ClassDeclaration of Class

and Class = {
    Name : Identifier;
    GenericTypeParameters : GenericTypeParameter list;
    BaseClass : DottedTypeName option
    ImplementedInterfaces : DottedTypeName list;
    Properties : PropertyDeclaration list;
    Constructor : Constructor option;
    FunctionDeclarations : Function list;
}

and DottedTypeName = Identifier list * CustomType

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
  Body : Statement list
}

//TODO: split into BuiltInType and CustomTypeSpec
and TypeSpec =
| Bool
| Char
| Int
| Float
| Double
| String
| Void
| Object
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
     | Object -> "System.Object"
     | Void -> failwith "generic can't be void"
   serializeTypeSpec ts

and CustomType = CustomType of Identifier * TypeSpec list

and Identifier = string

and Parameter = Identifier * TypeSpec

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
  Identifier * TypeSpec option * Expression
and VariableDeclaration =
| DeclarationWithInitialization of Identifier * Expression
| DeclarationWithType of Identifier * TypeSpec
| FullDeclaration of Identifier * TypeSpec * Expression



and IExpression = interface end
//TODO: http://theburningmonk.com/2012/03/f-extending-discriminated-unions-using-marker-interfaces/

and Expression =
| AssignmentExpression of Assignment
| BinaryExpression of Expression * BinaryOperator * Expression
| ExpressionWithInferredType of Expression * string option
| FunctionCallExpression of FunctionCall
| IdentifierExpression of Identifier
| ListInitializerExpression of Expression list
| LiteralExpression of Literal
| MemberExpression of MemberFunctionCall
| NewExpression of TypeSpec * Arguments
| StaticMemberExpression of TypeSpec * FunctionCall
| UnaryExpression of UnaryOperator * Expression

and MemberFunctionCall = MemberFunctionCall of Expression * Expression

and Assignment = Expression * Expression

and FunctionCall = FunctionCall of Identifier * TypeSpec list * Arguments

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

module Module =
    type Module = {
        Declarations : Declaration list
    } with
    static member (+) (m1 : Module, m2 : Module) =
          { Declarations = m1.Declarations @ m2.Declarations}
    member x.Classes =
        x.Declarations
        |> List.choose ( fun m ->
                         match m with
                         | ClassDeclaration c -> Some c
                         | _ -> None)
    member x.Functions =
        x.Declarations
        |> List.choose ( fun m ->
                         match m with
                         | FunctionDeclaration f -> Some f
                         | _ -> None)
    end
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
        { Declarations = classes |> List.map ClassDeclaration }
    let createDefault declarations = create "DEFAULT" declarations 
    let identity = {Declarations = []}
    let plus m1 m2 = {Declarations = m1.Declarations @ m2.Declarations}

