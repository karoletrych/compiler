module Compiler.Ast

type ProgramFile = {
    ModuleIdentifier : string option
    Declarations : Declaration<AstExpression> list
}

and Declaration<'Expression> =
| FunctionDeclaration of Function<'Expression>
| ClassDeclaration of Class<'Expression>

and Class<'Expression> = {
    Name : string
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

and Module<'Expression> = {
    Identifier : TypeIdentifier
    Functions : Function<'Expression> list
    Classes : ModuleClass<'Expression> list
}

and ModuleClass<'Expression> = {
    Identifier : TypeIdentifier
    BaseClass : TypeSpec option
    ImplementedInterfaces : TypeSpec list
    Properties : Property<'Expression> list
    Constructor : Constructor<'Expression> option
    Functions : Function<'Expression> list
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

and Expression<'Expression> =
| AssignmentExpression of 'Expression * 'Expression
| BinaryExpression of 'Expression * BinaryOperator * 'Expression
| FunctionCallExpression of FunctionCall<'Expression>
| IdentifierExpression of string
| ListInitializerExpression of 'Expression list
| LiteralExpression of Literal
| MemberExpression of MemberFunctionCall<'Expression>
| NewExpression of TypeSpec * 'Expression list
| StaticMemberExpression of TypeSpec * FunctionCall<'Expression>
| UnaryExpression of UnaryOperator * 'Expression

and AstExpression = AstExpression of Expression<AstExpression>

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

    let typeId t = 
        let (TypeIdentifier ti) = t
        ti

module Module =
    let create (moduleNamespace : string list) declarations =
        let nspace = moduleNamespace |> List.rev
        let identifier = 
            {
                Namespace = nspace.Tail;
                TypeName = {
                            Name = [nspace.Head];
                            GenericArguments = []
                           }
            }
        let functions = 
            declarations 
            |> List.choose ( fun m ->
                    match m with
                    | FunctionDeclaration f -> Some f
                    | _ -> None)
        let classes = 
            declarations 
            |> List.choose (fun m ->
                match m with
                | ClassDeclaration c -> Some c
                | _ -> None)
            |> List.map (fun c -> 
            {
                Identifier = 
                    {identifier 
                        with TypeName = 
                             {
                                identifier.TypeName 
                                with Name = 
                                        c.Name :: identifier.TypeName.Name
                             }
                     }
                BaseClass = c.BaseClass
                ImplementedInterfaces = c.ImplementedInterfaces
                Properties = c.Properties
                Constructor = c.Constructor
                Functions = c.Functions
            })
        { 
            Identifier = identifier
            Functions = functions
            Classes = classes
        }
    let createDefault declarations = create (["DEFAULT"]) declarations 
