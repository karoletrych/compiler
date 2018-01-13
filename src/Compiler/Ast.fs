module Compiler.Ast

type ProgramFile = {
    ModuleIdentifier : string list option
    Declarations : Declaration<AstExpression> list
}

and Declaration<'Expression> =
| FunctionDeclaration of Function<'Expression>
| ClassDeclaration of Class<'Expression>

and Class<'Expression> = {
    Name : string
    BaseClass : TypeSpec option
    Fields : Field<'Expression> list
    Constructors : Constructor<'Expression> list
    Functions : Function<'Expression> list
}

and Function<'Expression> = {
  Name : string
  Parameters : Parameter list
  ReturnType : TypeSpec option
  Body : Statement<'Expression> list
}

and Field<'Expression> = { 
  Type : TypeSpec
  Name : string
  Initializer : 'Expression option
  IsReadOnly : bool
}

and Constructor<'Expression> = {
    Parameters : Parameter list
    BaseClassConstructorCall : 'Expression list
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
| AssignmentStatement of Assignee<'Expression> * 'Expression
| CompositeStatement of Statement<'Expression> list
| LocalFunctionCallStatement of FunctionCall<'Expression>
| IfStatement of 'Expression * Statement<'Expression> * Statement<'Expression> option
| InstanceMemberFunctionCallStatement of 'Expression * FunctionCall<'Expression>
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

and Expression<'Expression> =
| AssignmentExpression of Assignee<'Expression> * 'Expression
| BinaryExpression of 'Expression * BinaryOperator * 'Expression
| LocalFunctionCallExpression of FunctionCall<'Expression>
| IdentifierExpression of string
| ListInitializerExpression of 'Expression list
| LiteralExpression of Literal
| InstanceMemberExpression of 'Expression * Member<'Expression>
| NewExpression of TypeSpec * 'Expression list
| StaticMemberExpression of TypeSpec * Member<'Expression>
| UnaryExpression of UnaryOperator * 'Expression

and AstExpression = AstExpression of Expression<AstExpression>
and Assignee<'Expression> = 
| IdentifierAssignee of string
| MemberFieldAssignee of 'Expression * string

and Member<'Expression> = 
| MemberFunctionCall of FunctionCall<'Expression>
| MemberField of string

and FunctionCall<'Expression> = {
        Name : string;
        GenericArguments : TypeSpec list;
        Arguments : 'Expression list
    }

and BinaryOperator =
| LogicalOr
| LogicalAnd
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
| FloatLiteral of single
| StringLiteral of string


// end of language grammar


and  Namespace = {
    Parts : string list
}

and Module<'Expression> = {
    Identifier : TypeIdentifier
    Functions : Function<'Expression> list
    Classes : ModuleClass<'Expression> list
}

and ModuleClass<'Expression> = {
    Identifier : TypeIdentifier
    BaseClass : TypeSpec option
    Fields : Field<'Expression> list
    Constructors : Constructor<'Expression> list
    Functions : Function<'Expression> list
}
and GenericParameter =
| DeclaredInParameterizedType
| DeclaredInOtherType of TypeIdentifier
| GenericArgument of TypeIdentifier

and TypeIdentifier = {
    Namespace : string list
    Name : string
    GenericParameters : GenericParameter list
    DeclaringType : TypeIdentifier option
    IsGenericParameter : bool
} 
with override ti.ToString() =
        let getName tId =
            let rec getNameRec tId =
                match tId.DeclaringType with
                | Some t -> getNameRec t @ [tId.Name]
                | None -> [tId.Name]
            (
            getNameRec tId 
            |> List.toArray 
            |> (fun segments -> System.String.Join("+", segments))
            )
            
        match ti.Namespace with
        | [] -> ""
        | ns -> 
            (ns |> List.toArray |> (fun parts -> System.String.Join(".", parts))) + "."
      + getName ti
      + if List.isEmpty ti.GenericParameters
        then ""
        else "[" + (ti.GenericParameters |> List.map (fun x -> x.ToString()) |> String.concat ",") + "]"

let getGenericArgument (genericParameter : GenericParameter) =
    let (GenericArgument arg) = genericParameter
    arg

module Identifier = 
    let rec fromDotNet (t : System.Type) = {
        Namespace = t.Namespace |> fun ns -> ns.Split('.') |> List.ofArray |> List.rev
        Name =  t.Name
        GenericParameters = 
            if t.IsGenericType
            then t.GetGenericArguments() 
                |> List.ofArray  
                |> List.map (fun parameter -> 
                                if (isNull parameter.DeclaringType)
                                then
                                    GenericArgument (fromDotNet parameter)
                                else
                                    if parameter.DeclaringType = t
                                    then DeclaredInParameterizedType
                                    else DeclaredInOtherType (fromDotNet parameter.DeclaringType))
            else []
        DeclaringType = 
            if not (t.DeclaringType |> isNull)
            then Some (fromDotNet t.DeclaringType)
            else None 
        IsGenericParameter = t.IsGenericParameter
    } 

    let object = fromDotNet typeof<obj> 
    let bool = fromDotNet typeof<bool> 
    let int = fromDotNet typeof<int> 
    let float = fromDotNet typeof<single> 
    let string = fromDotNet typeof<string> 
    let char = fromDotNet typeof<char>
    let double = fromDotNet typeof<double>
    let ``void`` = fromDotNet typeof<System.Void>
    let listOf (t : TypeIdentifier) = 
        let objList =  fromDotNet typeof<System.Collections.Generic.List<obj>>
        { 
            objList 
                with GenericParameters = [GenericArgument t]
        }
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
                Name = cts.Name + if List.isEmpty cts.GenericArgs then "" else "`" + (List.length cts.GenericArgs).ToString()
                GenericParameters = cts.GenericArgs |> List.map (fun ts -> GenericArgument (fromTypeSpec ts))
                DeclaringType = None
                IsGenericParameter = false
            }
        | TypeIdentifier(i) -> 
            i

    let typeId = 
        function 
        | TypeIdentifier ti  -> ti
        | BuiltInTypeSpec(_) -> failwith "Invalid operation"
        | CustomTypeSpec _ -> failwith "Invalid operation"
    let rec equalWithoutGeneric t1 t2 = 
        t1.Namespace = t2.Namespace
     && t1.Name = t2.Name
     && match (t1.DeclaringType, t2.DeclaringType) with
                        | Some t1DeclaringType, Some t2DeclaringType -> equalWithoutGeneric t1DeclaringType t2DeclaringType
                        | None, None -> true
                        | _ -> false
    
module Module =
    let create (moduleNamespace : string list) declarations =
        let nspace = moduleNamespace |> List.rev
        let identifier = 
            {
                Namespace = nspace.Tail;
                Name = nspace.Head;
                GenericParameters = [];
                DeclaringType = None;
                IsGenericParameter = false
            }
        let functions = 
            declarations 
            |> List.choose ( fun m ->
                    match m with
                    | FunctionDeclaration f -> Some f
                    | _ -> None)
        let classes = 
            declarations 
            |> List.choose (fun declaration ->
                match declaration with
                | ClassDeclaration c -> Some c
                | _ -> None)
            |> List.map (fun c -> 
            {
                Identifier = 
                    {
                        Name = c.Name
                        DeclaringType = Some identifier
                        GenericParameters = []
                        Namespace = nspace.Tail
                        IsGenericParameter = false
                     }
                BaseClass = c.BaseClass
                Fields = c.Fields
                Constructors = c.Constructors
                Functions = c.Functions
            })
        { 
            Identifier = identifier
            Functions = functions
            Classes = classes
        }

let operatorMethodName =
    function
    | Plus -> "op_Addition"
    | LogicalOr -> "op_LogicalOr"
    | LogicalAnd -> "op_LogicalAnd"
    | Equal ->  "op_Equality"
    | NotEqual ->  "op_Inequality"
    | LessEqual ->  "op_LessThanOrEqual"
    | Less ->  "op_LessThan"
    | GreaterEqual ->  "op_GreaterThanOrEqual"
    | Greater ->  "op_GreaterThan"
    | Minus ->  "op_Subtraction"
    | Multiplication ->  "op_Multiply"
    | Division ->  "op_Division"
    | Remainder ->  "op_Modulus"