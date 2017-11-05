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
    let rec fromTypeSpec (typeSpec : TypeSpec) = 
        let builtInTypeSpec =
            function
            | Bool -> {Namespace = ["System"]; TypeName = {Name = ["Bool"]; GenericArguments = []}} 
            | Char -> {Namespace = ["System"]; TypeName = {Name = ["Char"]; GenericArguments = []}} 
            | Int -> {Namespace = ["System"]; TypeName = {Name = ["Int32"]; GenericArguments = []}} 
            | Float -> {Namespace = ["System"]; TypeName = {Name = ["Single"]; GenericArguments = []}} 
            | Double -> {Namespace = ["System"]; TypeName = {Name = ["Double"]; GenericArguments = []}} 
            | String -> {Namespace = ["System"]; TypeName = {Name = ["String"]; GenericArguments = []}} 
            | Void -> {Namespace = ["System"]; TypeName = {Name = ["Void"]; GenericArguments = []}} 
            | Object  -> {Namespace = ["System"]; TypeName = {Name = ["Object"]; GenericArguments = []}} 
        match typeSpec with
        | BuiltInTypeSpec bits -> builtInTypeSpec bits
        | CustomTypeSpec cts -> 
        {
            Namespace = cts |> fst |> List.rev
            TypeName = 
            {
                Name = cts |> snd |> (fun t -> [t.Name]);
                GenericArguments = cts |> snd |> (fun t -> t.GenericArgs |> List.map fromTypeSpec)
            }
        } 
    let rec fromClassDeclaration (c : Class) = {
        Namespace = c.Name 
                    |> fun c -> c.Split([|"::"|], System.StringSplitOptions.None) 
                    |> List.ofArray
                    |> List.rev
                    |> List.tail
                    |> List.rev
        TypeName = 
        {
            Name =  [c.Name |> fun c -> c.Split([|"::"|], System.StringSplitOptions.None) 
                            |> List.ofArray
                            |> List.last]
            GenericArguments = [] // TODO: fix
        }
    } 

    let rec fromDotNet (t : System.Type) = {
        Namespace = t.Namespace |> fun ns -> ns.Split('.') |> List.ofArray |> List.rev
        TypeName = 
        {
            Name =  [ (if t.Name.Contains("`") then t.Name.Substring(0, t.Name.LastIndexOf("`")) else t.Name) ]
            GenericArguments = if t.IsGenericTypeDefinition then t.GetGenericArguments() |> List.ofArray |> List.map fromDotNet else []
        }
    } 


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
