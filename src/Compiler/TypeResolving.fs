module Compiler.TypeResolving

open Compiler.Types
open CompilerResult
open AstProcessing
open Ast

let resolveTypeSpec 
    (types : List<TypeIdentifier>)
    (currentTypeId) 
    (typeSpec : TypeSpec) =
    let id = typeSpec |> Identifier.fromTypeSpec
    let typesInCurrentNamespace =
        types
        |> List.filter 
            (fun id -> id.Namespace = currentTypeId.Namespace)
        
    match typesInCurrentNamespace |> List.tryFind (fun x -> x.TypeName = id.TypeName) with
    | Some id -> Result.succeed (TypeIdentifier id)
    | None -> 
        match types |> List.tryFind (fun x -> x = id) with
        | Some id -> Result.succeed (TypeIdentifier id)
        | None -> Result.failure (CannotResolveType typeSpec)


let resolveExpression resolveType expression =
    let assignment (e1,e2) = Result.map2 (fun e1 e2 -> AssignmentExpression(e1, e2)) e1 e2
    let binary (e1, op, e2) = Result.map2 (fun e1 e2 -> BinaryExpression(e1, op, e2)) e1 e2
    let inferredType t = failwith "unexpected"
    let functionCall (name,args,generics) = 
        let argsResult = args |> Result.merge 
        let genericsResult =
            generics 
            |> List.map resolveType 
            |> Result.merge
        (argsResult, genericsResult) 
        ||> Result.map2 (fun args generics ->
            FunctionCallExpression {Name = name; Arguments = args; GenericArguments = generics}) 
    let identifier = IdentifierExpression >> Result.succeed
    let literal = LiteralExpression >> Result.succeed
    let listInitializer list = list |> Result.merge |> Result.map ListInitializerExpression 
    let memberExpression (e1, e2) = Result.map2 ( (fun e1 e2 -> MemberExpression (MemberFunctionCall(e1, e2)) ) ) e1 e2
    let newExpression (t, args) = Result.map2 (fun t args -> NewExpression(t,args)) (resolveType t) (Result.merge args)
    let staticMember (t, (name,args,generics)) =
        let argsResult = args |> Result.merge 
        let genericsResult = generics |> List.map resolveType |> Result.merge
        (resolveType t, argsResult, genericsResult) 
        |||> Result.map3 (fun t args generics -> StaticMemberExpression(t, {Name = name; Arguments = args; GenericArguments = generics}))

    let unary (op,e) = Result.map (fun e -> UnaryExpression(op, e)) e
    expressionCata 
        assignment 
        binary 
        inferredType 
        functionCall 
        identifier 
        literal 
        listInitializer 
        memberExpression
        newExpression 
        staticMember 
        unary 
        expression

let resolveStatement resolveExpression resolveType statement : CompilerResult<Statement> =
    let functionCall (id, args, generics) = 
        let args = args |> List.map resolveExpression |> Result.merge
        let generics =  generics |> List.map resolveType |> Result.merge
        (args, generics)
        ||> Result.map2 (
            (fun args generics ->
                FunctionCallStatement {Name = id; Arguments = args; GenericArguments = generics})) 
    let staticFunctionCall (t, (id, args, generics)) =
        let args = args |> List.map resolveExpression |> Result.merge
        let generics =  generics |> List.map resolveType |> Result.merge
        (resolveType t, args, generics)
        |||>Result.map3 (
            (fun t args generics ->
                StaticFunctionCallStatement(t, {Name = id; Arguments = args; GenericArguments = generics}))) 
    let valueDeclaration (id, t : TypeSpec option, e) = (Result.mapOption resolveType t, resolveExpression e) ||> Result.map2 (fun t e -> ValueDeclaration(id,t,e))
    let declarationWithInitialization (id, e) = resolveExpression e |> Result.map (fun e -> VariableDeclaration ( DeclarationWithInitialization(id,e)))
    let declarationWithType (id, t) = resolveType t |> Result.map (fun t -> VariableDeclaration ( DeclarationWithType(id,t)) )
    let fullVariableDeclaration (id, t, e) = (resolveType t, resolveExpression e) ||> Result.map2 (fun t e -> VariableDeclaration( FullDeclaration(id,t,e)) )
    let composite stmts = stmts |> Result.merge |> Result.map (fun stmts -> CompositeStatement(stmts)) 
    let returnStatement e = (Result.mapOption resolveExpression e) |> Result.map (fun e -> ReturnStatement(e)) 
    let assignment (e1, e2) = (resolveExpression e1, resolveExpression e2) ||> Result.map2 (fun e1 e2 -> AssignmentStatement(e1, e2)) 
    let breakStatement = Result.succeed BreakStatement
    statementCata
        functionCall
        staticFunctionCall
        valueDeclaration
        declarationWithInitialization
        declarationWithType
        fullVariableDeclaration
        composite
        returnStatement
        assignment
        breakStatement
        statement

let resolveParameters resolveType = (fun p -> (p |> snd |> resolveType |> Result.map (fun t -> (fst p, t))))
let resolveFunction resolveStatement resolveType func = 
    let parameters = 
        func.Parameters 
        |> List.map (resolveParameters resolveType)
        |> Result.merge
    //TODO: GenericParameters = func.GenericParameters |> List.map scanTypeSpec
    let returnType = 
        func.ReturnType 
        |> Result.mapOption resolveType;
    let body = 
        func.Body 
        |> List.map resolveStatement 
        |> Result.merge
    (parameters, returnType, body)
    |||> Result.map3 
        (fun p r b -> 
        {
            Name = func.Name;
            Parameters = p; 
            ReturnType = r; 
            Body = b; 
            GenericParameters = []
        })



let resolveClass knownTypes (clas : Class) =
    let classId = Identifier.fromClassDeclaration clas
    let resolveTypeSpec = resolveTypeSpec knownTypes classId
    let resolveExpression = resolveExpression resolveTypeSpec
    let resolveStatement = resolveStatement resolveExpression resolveTypeSpec
    let resolveFunction = resolveFunction resolveStatement resolveTypeSpec

    let baseClass = Result.mapOption resolveTypeSpec clas.BaseClass
    let interfaces = clas.ImplementedInterfaces |> List.map resolveTypeSpec |> Result.merge
    let properties = clas.Properties 
                     |> List.map (fun p -> 
                            let init = p.Initializer |> Result.mapOption resolveExpression; 
                            let t = resolveTypeSpec p.Type
                            Result.map2 
                                (fun i t -> 
                                { 
                                    Name = p.Name;
                                    Initializer = i;
                                    Type = t
                                }) 
                                init 
                                t
                        )
                     |> Result.merge
    let constructor = clas.Constructor 
                      |> Result.mapOption (fun c ->
                            let parameters = 
                                c.Parameters 
                                |> List.map (resolveParameters resolveTypeSpec)
                                |> Result.merge
                            let baseClassConstructorCall = 
                                c.BaseClassConstructorCall 
                                |> List.map resolveExpression 
                                |> Result.merge
                            let statements = 
                                c.Statements 
                                |> List.map resolveStatement
                                |> Result.merge
                            (parameters, baseClassConstructorCall, statements)
                            |||> Result.map3 (fun p b s -> {Parameters = p; BaseClassConstructorCall = b; Statements = s})
                       )
    let functionDeclarations = clas.FunctionDeclarations 
                               |> List.map resolveFunction
                               |> Result.merge
    (fun baseClass interfaces properties constructor functionDeclarations -> 
    {
        Name = clas.Name
        GenericTypeParameters = []
        BaseClass = baseClass
        ImplementedInterfaces = interfaces
        Properties = properties
        Constructor = constructor
        FunctionDeclarations = functionDeclarations
    })
    <!> baseClass <*> interfaces <*> properties <*> constructor <*> functionDeclarations


let resolve (modul : Module, knownTypes : List<TypeIdentifier>)= 
    modul.Classes
    |> List.map (resolveClass knownTypes)
    |> Result.merge
    |> Result.map (fun classes -> {Classes = classes})
    |> Result.map (fun c -> (c, knownTypes))
