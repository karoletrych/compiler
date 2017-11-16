module Compiler.Ast

type Module = {
    Name : string
    Functions : Function list
    Classes : Class list
}

and Declaration =
| FunctionDeclaration of Function
| ClassDeclaration of Class

and Class = {
    Name : string;
    GenericTypeParameters : GenericTypeParameter list;
    BaseClass : TypeSpec option
    ImplementedInterfaces : TypeSpec list;
    Properties : PropertyDeclaration list;
    Constructor : Constructor option;
    Functions : Function list;
}

and PropertyDeclaration = { 
  Name : string;
  Type : TypeSpec;
  Initializer : Expression option
}

and Constructor = {
    Parameters : Parameter list;
    BaseClassConstructorCall : Expression list;
    Statements : Statement list;
}

and GenericTypeParameter = GenericTypeParameter of string // TODO: it should be TypeParameter example: A<IEnumerable<T>> 

and Function = {
  Name : string;
  GenericParameters : GenericTypeParameter list;
  Parameters : Parameter list;
  ReturnType : TypeSpec option;
  Body : Statement list
}

and TypeSpec =
| BuiltInTypeSpec of BuiltInTypeSpec
| CustomTypeSpec of string list * CustomType
| TypeIdentifier of TypeIdentifier

and BuiltInTypeSpec = 
| Bool
| Char
| Int
| Float
| Double
| String
| Void
| Object


and CustomType = { 
    Name : string 
    GenericArgs : TypeSpec list
}

and Parameter = string * TypeSpec

and Statement =
| AssignmentStatement of Expression * Expression
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

and Expression =
| AssignmentExpression of Expression * Expression
| BinaryExpression of Expression * BinaryOperator * Expression
| FunctionCallExpression of FunctionCall
| IdentifierExpression of string
| ListInitializerExpression of Expression list
| LiteralExpression of Literal
| MemberExpression of MemberFunctionCall
| NewExpression of TypeSpec * Expression list
| StaticMemberExpression of TypeSpec * FunctionCall
| UnaryExpression of UnaryOperator * Expression
| InferredTypeExpression of Expression * TypeIdentifier

and MemberFunctionCall = MemberFunctionCall of Expression * Expression


and FunctionCall = {
     Name : string;
     GenericArguments : TypeSpec list;
     Arguments : Expression list
     }


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

and  Namespace = {
    Parts : string list
}

and TypeName = {
    Name : string list
    GenericArguments : TypeIdentifier list
}

and TypeIdentifier = {
    Namespace : string list
    TypeName : TypeName
} 
with member x.GenericArgumentsNumber = 
        x.TypeName.GenericArguments |> List.length
     override ti.ToString() =
       (ti.Namespace |> List.toArray |> (fun strs -> System.String.Join("::", strs)))
      + (ti.TypeName.Name |> List.toArray |> (fun strs -> System.String.Join("+",  strs)))
      + if List.isEmpty ti.TypeName.GenericArguments
        then ""
        else "`" + (List.length ti.TypeName.GenericArguments).ToString() + "[" + (ti.TypeName.GenericArguments |> List.map (fun x -> x.ToString()) |> String.concat ",") + "]"
    

module Identifier = 

    let rec fromDotNet (t : System.Type) = {
        Namespace = t.Namespace |> fun ns -> ns.Split('.') |> List.ofArray |> List.rev
        TypeName = 
        {
            Name =  [ (
                        if t.Name.Contains("`") 
                        then t.Name.Substring(0, t.Name.LastIndexOf("`"))
                        else t.Name) ]
            GenericArguments = 
                if t.IsGenericTypeDefinition 
                then t.GetGenericArguments() |> List.ofArray |> List.map fromDotNet 
                else []
        }
    } 

    let object = fromDotNet typeof<obj> 
    let bool = fromDotNet typeof<bool> 
    let int = fromDotNet typeof<int> 
    let float = fromDotNet typeof<single> 
    let string = fromDotNet typeof<string> 
    let list t = 
        let objList =  fromDotNet typeof<System.Collections.Generic.List<obj>>
        { objList 
            with TypeName = 
                            {
                            Name = objList.TypeName.Name;
                            GenericArguments = [t]
                            }}
    let rec fromTypeSpec (typeSpec : TypeSpec) = 
        let builtInTypeSpec =
            function
            | Bool -> bool
            | Char -> fromDotNet typeof<char>
            | Int -> int
            | Float -> float
            | Double -> fromDotNet typeof<double>
            | String -> string
            | Void -> fromDotNet typeof<unit>
            | Object  -> object
        match typeSpec with
        | BuiltInTypeSpec bits -> builtInTypeSpec bits
        | CustomTypeSpec (ns, cts) -> 
        {
            Namespace = ns |> List.rev
            TypeName = 
            {
                Name = cts |>  (fun t -> [t.Name]);
                GenericArguments = cts |> (fun t -> t.GenericArgs |> List.map fromTypeSpec)
            }
        } 
    let rec fromClassDeclaration (c : Class) = 
        let splittedName = 
                    c.Name 
                    |> (fun n -> n.Split([|"::"|], System.StringSplitOptions.None))
                    |> List.ofArray
        {
            Namespace = splittedName
                        |> List.rev
                        |> List.tail
                        |> List.rev
            TypeName = 
            {
                Name =  splittedName
                        |> List.last
                        |> fun c -> c.Split([|"+"|], System.StringSplitOptions.None)
                        |> List.ofArray
                        |> List.rev
                GenericArguments = [] // TODO: fix
            }
        } 


    let rec fromModule (m : Module) =
        let splittedName = 
                m.Name 
                |> (fun n -> n.Split([|"::"|], System.StringSplitOptions.None))
                |> List.ofArray
        {
            Namespace = splittedName
                        |> List.rev
                        |> List.tail
                        |> List.rev
            TypeName = 
            {
                Name =  [splittedName |> List.last]
                GenericArguments = [] 
            }
        } 

    let typeId t = 
        let (TypeIdentifier ti) = t
        ti

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
        let internalClasses = 
            classes 
            |> List.map (fun c -> {c with Name = moduleName + "+" + c.Name })
        { 
          Name = moduleName
          Functions = functions
          Classes = internalClasses
        }
    let createDefault declarations = create "DEFAULT" declarations 
