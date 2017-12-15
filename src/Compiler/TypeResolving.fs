module Compiler.TypeResolving

open CompilerResult
open AstProcessing
open Ast
let merge options = 
    match options with
            | [] -> Some []
            | list -> if list |> List.forall Option.isSome
                      then list |> List.map Option.get |> Some
                      else None

let localTypeIsEqual specifiedType knownType =
let typeIsEqual specifiedType knownType =
let rec resolveTypeIdentifier 
    types
    (currentTypeId : TypeIdentifier)
    tId =
    let resolveTypeIdentifier = resolveTypeIdentifier types currentTypeId
    let typesInCurrentModule =
        types
        |> List.filter 
            (fun id -> id.Namespace = currentTypeId.Namespace && id.TypeName.Name |> List.tail = tId.TypeName.Name |> List.tail)
    // find fully qualified name

    tId.TypeName.GenericArguments 
        |> List.map resolveTypeIdentifier
        |> merge
        |> Option.bind 
            (fun generics ->
                typesInCurrentModule 
                |> List.tryFind(fun x -> localTypeIsEqual tId x)
                |> Option.map (fun t ->
                    {t with TypeName = {t.TypeName with GenericArguments = generics}}
                ))
    // or look through locally visible types
    |> Option.orElse 
        (tId.TypeName.GenericArguments 
            |> List.map resolveTypeIdentifier
            |> merge
            |> Option.bind 
                (fun generics ->
                    types 
                    |> List.tryFind(fun x -> typeIsEqual tId x)
                    |> Option.map (fun t ->
                        {t with TypeName = {t.TypeName with GenericArguments = generics}}
                    )))
                        

let private resolveTypeSpec 
    types
    currentTypeId
    typeSpec =
    match resolveTypeIdentifier types currentTypeId (Identifier.fromTypeSpec typeSpec) with
    | Some id -> Result.succeed (TypeIdentifier id)
    | None -> Result.failure (TypeNotFound typeSpec)

let private resolveExpression resolveType expression : CompilerResult<AstExpression>=
    let resolveFunctionCall (name, args, generics) =
        let argsResult = args |> Result.merge 
        let genericsResult =
            generics 
            |> List.map resolveType 
            |> Result.merge
        Result.map2 (fun args generics -> {Name = name; Arguments = args; GenericArguments = generics}) 
            argsResult genericsResult

    let assignment ((e1, i), e2) = Result.map2 (fun e1 e2 -> AssignmentExpression(MemberFieldAssignee(e1, i), e2)) e1 e2
    let binary (e1, op, e2) = Result.map2 (fun e1 e2 -> BinaryExpression(e1, op, e2)) e1 e2
    let functionCall fc = 
        (resolveFunctionCall fc)
        |> Result.map (fun fc -> LocalFunctionCallExpression(fc)) 
        
    let identifier = IdentifierExpression >> Result.succeed
    let literal = LiteralExpression >> Result.succeed
    let listInitializer list = list |> Result.merge |> Result.map ListInitializerExpression 
    let memberFunctionCall (e1, fc) = 
        (e1, resolveFunctionCall fc)
        ||> Result.map2 
            (fun e1 fc -> 
                InstanceMemberExpression (e1, MemberFunctionCall(fc))) 
    let memberField (e,f) = e |> Result.map (fun e -> (e,f |> MemberField) |> InstanceMemberExpression)
    let newExpression (t, args) = Result.map2 (fun t args -> NewExpression(t,args)) (resolveType t) (Result.merge args)
    let staticMemberFunctionCall (t, fc) =
        (resolveType t, resolveFunctionCall fc) 
        ||> Result.map2 (fun t fc -> StaticMemberExpression(t, MemberFunctionCall(fc)))
    let staticMemberField (t, f) = 
        (resolveType t) 
        |> Result.map (fun t -> (t, MemberField f) |> StaticMemberExpression)

    let unary (op,e) = Result.map (fun e -> UnaryExpression(op, e)) e
    expressionCata 
        (assignment >> Result.map AstExpression)
        (binary >> Result.map AstExpression)
        (functionCall >> Result.map AstExpression)
        (identifier >> Result.map AstExpression)
        (literal >> Result.map AstExpression)
        (listInitializer >> Result.map AstExpression)
        (memberFunctionCall >> Result.map AstExpression)
        (memberField >> Result.map AstExpression)
        (newExpression >> Result.map AstExpression)
        (staticMemberFunctionCall >> Result.map AstExpression)
        (staticMemberField >> Result.map AstExpression)
        (unary >> Result.map AstExpression)
        expression

let private resolveStatement resolveExpression resolveType statement =
    let functionCall (id, args, generics) = 
        let args = args |> List.map resolveExpression |> Result.merge
        let generics =  generics |> List.map resolveType |> Result.merge
        (args, generics)
        ||> Result.map2 (
            (fun args generics ->
                LocalFunctionCallStatement {Name = id; Arguments = args; GenericArguments = generics})) 
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
    let assignment ((e1, i), e2) = (resolveExpression e1, resolveExpression e2) ||> Result.map2 (fun e1 e2 -> AssignmentStatement(MemberFieldAssignee(e1,i), e2)) 
    let identifierAssignment (i, e2) = (resolveExpression e2) |> Result.map (fun e2 -> AssignmentStatement(IdentifierAssignee(i), e2)) 
    let breakStatement = Result.succeed BreakStatement
    let ifStatement (e,s,elseS) = 
        match elseS with
        | Some elseS ->
            (resolveExpression e, s, elseS)
            |||>Result.map3 (fun e s elseS -> IfStatement(e, s, Some elseS))
        | None ->
            (resolveExpression e, s)
            ||>Result.map2 (fun e s -> IfStatement(e, s, None))
    let whileStatement (e, s) = 
        (resolveExpression e, s)
        ||> Result.map2 (fun e s -> WhileStatement(e,s))
    let instanceFunctionCall (e, fc : FunctionCall<AstExpression>) =
        (resolveExpression e, fc.Arguments |> List.map resolveExpression |> Result.merge)
        ||> Result.map2 (fun e args -> InstanceMemberFunctionCallStatement(e, {Name = fc.Name; Arguments = args; GenericArguments = fc.GenericArguments}))


    statementCata
        functionCall
        staticFunctionCall
        valueDeclaration
        declarationWithInitialization
        declarationWithType
        fullVariableDeclaration
        composite
        returnStatement
        identifierAssignment
        assignment
        breakStatement
        ifStatement
        whileStatement
        instanceFunctionCall
        statement

let private resolveParameters resolveType = (fun p -> (p |> snd |> resolveType |> Result.map (fun t -> (fst p, t))))
let private resolveFunction resolveStatement resolveType (func : Function<AstExpression>) = 
    let parameters = 
        func.Parameters 
        |> List.map (resolveParameters resolveType)
        |> Result.merge
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
        })

let private resolveClass resolveTypeSpec resolveExpression resolveStatement resolveFunction clas =
    let baseClass = Result.mapOption resolveTypeSpec clas.BaseClass
    let properties = clas.Fields 
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
    let constructors = clas.Constructors 
                      |> List.map (fun c -> 
                                let parameters = 
                                    c.Parameters 
                                    |> List.map (resolveParameters resolveTypeSpec)
                                    |> Result.merge
                                let baseClassConstructorCall = 
                                    c.BaseClassConstructorCall 
                                    |> List.map resolveExpression 
                                    |> Result.merge
                                let statements = 
                                    c.Body 
                                    |> List.map resolveStatement
                                    |> Result.merge
                                (parameters, baseClassConstructorCall, statements)
                                |||> Result.map3 (fun p b s -> {Parameters = p; BaseClassConstructorCall = b; Body = s})
                       )
                       |> Result.merge
    let functionDeclarations = 
        clas.Functions 
       |> List.map resolveFunction
       |> Result.merge
    (fun baseClass fields constructor functionDeclarations -> 
    {
        Identifier = clas.Identifier
        BaseClass = baseClass
        Fields = fields
        Constructors = constructor
        Functions = functionDeclarations
    })
    <!> baseClass <*> properties <*> constructors <*> functionDeclarations

let private resolveModuleFunction knownTypes (modul : Module<AstExpression>) = 
    let resolveTypeSpec = resolveTypeSpec knownTypes modul.Identifier
    let resolveExpression = resolveExpression resolveTypeSpec
    let resolveStatement = resolveStatement resolveExpression resolveTypeSpec
    resolveFunction resolveStatement resolveTypeSpec
let private resolveModuleClass knownTypes clas = 
    let resolveTypeSpec = resolveTypeSpec knownTypes clas.Identifier
    let resolveExpression = resolveExpression resolveTypeSpec
    let resolveStatement = resolveStatement resolveExpression resolveTypeSpec
    let resolveFunction = resolveFunction resolveStatement resolveTypeSpec
    resolveClass resolveTypeSpec resolveExpression resolveStatement resolveFunction clas


let private resolveModule knownTypes (modul : Module<AstExpression>) = 
    let resolveModuleFunction = resolveModuleFunction knownTypes modul
    let resolveClass = resolveModuleClass knownTypes
    
    Result.map2 
        (fun functions classes -> {modul with Classes = classes; Functions = functions})
            (modul.Functions 
                |> List.map resolveModuleFunction 
                |> Result.merge)
            (modul.Classes 
                |> List.map resolveClass 
                |> Result.merge)

let resolve (modules : Module<AstExpression> list, knownTypes : TypeIdentifier list) =
    modules
    |> List.map (resolveModule knownTypes)
    |> Result.merge
