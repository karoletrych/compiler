module Compiler.Ast

type Module<'Expression> = {
    Name : string
    Functions : Function<'Expression> list
    Classes : Class<'Expression> list
}

and Declaration<'Expression> =
| FunctionDeclaration of Function<'Expression>
| ClassDeclaration of Class<'Expression>

and Class<'Expression> = {
    Name : string
    GenericTypeParameters : GenericTypeParameter list
    BaseClass : TypeSpec option
    ImplementedInterfaces : TypeSpec list
    Properties : Property<'Expression> list
    Constructor : Constructor<'Expression> option
    Functions : Function<'Expression> list
}

and Property<'Expression> = { 
  Type : TypeSpec
  Name : string
  Initializer : 'Expression option
}

and Constructor<'Expression> = {
    Parameters : Parameter list
    BaseClassConstructorCall : 'Expression list
    Statements : Statement<'Expression> list
}

and GenericTypeParameter = GenericTypeParameter of string // TODO: it should be TypeParameter example: A<IEnumerable<T>> 

and Function<'Expression> = {
  Name : string
  GenericParameters : GenericTypeParameter list
  Parameters : Parameter list
  ReturnType : TypeSpec option
  Body : Statement<'Expression> list
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

and Statement<'Expression> =
| AssignmentStatement of 'Expression * 'Expression
| BreakStatement
| CompositeStatement of Statement<'Expression> list
| FunctionCallStatement of FunctionCall<'Expression>
| IfStatement of 'Expression * Statement<'Expression> * Statement<'Expression> option
| MemberFunctionCallStatement of MemberFunctionCall<'Expression>
| ReturnStatement of 'Expression option
| StaticFunctionCallStatement of TypeSpec * FunctionCall<'Expression>
| VariableDeclaration of VariableDeclaration<'Expression>
| ValueDeclaration of ValueDeclaration<'Expression>
| WhileStatement of 'Expression * Statement<'Expression> 

and ValueDeclaration<'Expression> =
  string * TypeSpec option * 'Expression
and VariableDeclaration<'Expression> =
| DeclarationWithInitialization of string * 'Expression
| DeclarationWithType of string * TypeSpec
| FullDeclaration of string * TypeSpec * 'Expression

and MemberFunctionCall<'Expression> = MemberFunctionCall of 'Expression * 'Expression


and FunctionCall<'Expression> = {
     Name : string;
     GenericArguments : TypeSpec list;
     Arguments : 'Expression list
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
        match ti.Namespace with
        | [] -> ""
        | ns -> 
            (ns |> List.toArray |> (fun parts -> System.String.Join(".", parts))) + "."
      + (ti.TypeName.Name |> List.toArray |> (fun parts -> System.String.Join("+",  parts)))
      + if List.isEmpty ti.TypeName.GenericArguments
        then ""
        else "`" + (List.length ti.TypeName.GenericArguments).ToString() + "[" + (ti.TypeName.GenericArguments |> List.map (fun x -> x.ToString()) |> String.concat ",") + "]"

and Expression =
| AssignmentExpression of Expression * Expression
| BinaryExpression of Expression * BinaryOperator * Expression
| FunctionCallExpression of FunctionCall<Expression>
| IdentifierExpression of string
| ListInitializerExpression of Expression list
| LiteralExpression of Literal
| MemberExpression of MemberFunctionCall<Expression>
| NewExpression of TypeSpec * Expression list
| StaticMemberExpression of TypeSpec * FunctionCall<Expression>
| UnaryExpression of UnaryOperator * Expression
    


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
    let char = fromDotNet typeof<char>
    let double = fromDotNet typeof<double>
    let ``void`` = fromDotNet typeof<System.Void>
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
            | Char -> char
            | Int -> int
            | Float -> float
            | Double -> double
            | String -> string
            | Void -> ``void``
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
    let rec fromClassDeclaration (c : Class<'Expression>) = 
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

    let rec fromModule (m : Module<'Expression>) =
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
