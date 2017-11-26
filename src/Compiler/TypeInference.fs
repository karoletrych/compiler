module Compiler.TypeInference

open Compiler.Ast
open Types
open CompilerResult
open AstProcessing


type InferredTypeExpression = InferredTypeExpression of Expression<InferredTypeExpression> * TypeIdentifier
let unwrapExpression expr = 
    let (AstExpression expr) = expr
    expr

let builtInType (types : Map<TypeIdentifier, Type>) (t : TypeSpec) =
    types.[t |> Identifier.fromTypeSpec]


let private leastUpperBound (knownTypes : Map<TypeIdentifier, Type>) types=
    let rec allAncestors (t : Type) : Type list  =
        match t.BaseTypes with 
        | [] -> [] 
        | b -> b @ (b |> List.collect (fun t -> (allAncestors t)))

    let children allNodes parentNode = 
                allNodes 
                |> List.filter (fun (t : Type) -> 
                    t.BaseTypes |> List.contains parentNode
                    )

    let rec longestPath (root : Type) allNodes =
        let childrenOf = children allNodes
        let isALeaf = childrenOf root = List.empty
        if isALeaf 
        then
            ([root], 0)
        else
            let edges = childrenOf root |> List.map (fun child ->
                let (p, l) = longestPath child allNodes
                (child :: p, l + 1)
                )
            edges |> List.maxBy snd

    let longest = longestPath (builtInType knownTypes (BuiltInTypeSpec Object))

    types
        |> List.map (fun t -> (t::allAncestors t))
        |> List.map Set.ofList
        |> Set.intersectMany
        |> Set.toList
        |> List.distinct
        |> longest
        |> fst
        |> List.last

let matchingFunction (t, (name, args, generics), isStatic) = 
        t.Methods 
        |> List.tryFind 
            (fun f -> f.Name = name
                    && f.Parameters |> List.map (fun p -> p.Type) = args
                    // && List.length f.GenericParameters = List.length generics  TODO:
                    && f.IsStatic = isStatic)

let private findFunctionTypeInClass t ((name, args, generics), isStatic) : CompilerResult<TypeIdentifier> =
    match matchingFunction (t, (name, args, generics), isStatic) with
    | Some f -> match f.ReturnType with
                | Some ret -> Result.succeed ret
                | None -> failwith "TODO: "
    | None -> Result.failure (FunctionNotFound(t.Identifier, name,args,generics))

let private findFieldTypeInClass t (name, isStatic) = 
    t.Fields 
    |> List.tryFind (fun f-> f.FieldName = name && f.IsStatic = isStatic)
    |> function
       | Some field -> Result.succeed field.Type
       | None -> Result.failure (FieldNotFound(t.Identifier, name))


let typeOfLiteral = 
    (function
    | BoolLiteral _-> Identifier.bool
    | IntLiteral _-> Identifier.int
    | FloatLiteral _-> Identifier.float
    | StringLiteral _-> Identifier.string
    ) >> Result.succeed

let private inferExpression currentClass (knownTypes : Map<TypeIdentifier, Types.Type>) leastUpperBound lookupLocalVariable expression =
    let assignment (t1, _) = t1
    let binary (t1, op, t2) =
        let commonType t1 t2 = 
            if t1 = t2 
            then Result.succeed t1 
            else Result.failure 
                    (CannotInferType 
                        (sprintf "Cannot infer type of binary expression of types: %s and %s" (t1.ToString()) (t2.ToString())))
        match op,t1,t2 with
        | _, Failure error1, Failure error2 -> Failure (error1 @ error2)
        | _, Failure error, _ -> Failure error
        | _, _, Failure error -> Failure error
        | Plus, Success e1, Success e2 -> commonType e1 e2
        | Minus, Success e1, Success e2 -> commonType e1 e2
        | Multiplication, Success e1, Success e2 -> commonType e1 e2
        | Division, Success e1, Success e2 -> commonType  e1 e2
        | Remainder, Success e1, Success e2 -> commonType e1 e2
        | _ -> Success Identifier.bool
    let functionCall (name, args, generics) = 
        args 
        |> Result.merge 
        |> Result.bind (fun argTypes -> 
                            findFunctionTypeInClass currentClass ((name, argTypes, generics), currentClass.IsStatic))
    let identifier = lookupLocalVariable
    let literal = 
        (
        function 
        | BoolLiteral _ -> Identifier.bool
        | IntLiteral _-> Identifier.int
        | FloatLiteral _-> Identifier.float
        | StringLiteral _-> Identifier.string
        ) >> Result.succeed
    let listInitializer list = 
        list 
        |> Result.merge 
        |> Result.map leastUpperBound 
        |> Result.map Identifier.list
    let memberFunctionCall (callee, (name, args, generics)) = 
        (callee, args |> Result.merge) 
        ||> Result.bind2 (fun calleeType args -> findFunctionTypeInClass knownTypes.[calleeType] ((name,args,generics), false))
    let memberField (callee, f) = 
        callee 
        |> Result.bind (fun calleeType -> findFieldTypeInClass knownTypes.[calleeType] (f, false))
    let newExpression (t, _) = 
        Result.succeed (Identifier.typeId t)

    let typeSpecToTypeId t =
        knownTypes.[Identifier.typeId t]
    let staticMemberFunctionCall (t, (name, args, generics)) = 
        args 
        |> Result.merge
        |> Result.bind 
            (fun args -> findFunctionTypeInClass 
                            (typeSpecToTypeId t) ((name, args, generics), true))
    let staticMemberField (t, f) = 
        findFieldTypeInClass (typeSpecToTypeId t)  (f, true)

    let unary (_, t) = t
    expressionCata 
        assignment
        binary 
        functionCall 
        identifier 
        literal 
        listInitializer 
        memberFunctionCall 
        memberField
        newExpression 
        staticMemberFunctionCall 
        staticMemberField
        unary 
        expression

let rec private annotate 
    (inferExpression : AstExpression -> CompilerResult<TypeIdentifier>) 
    (expr : AstExpression) : CompilerResult<InferredTypeExpression> = 
    let recurse = annotate inferExpression
    let unwrapped = unwrapExpression expr
    let annotateFunction fc =
        let args = fc.Arguments |> List.map recurse |> Result.merge
        (args |> Result.map (fun args -> {Name = fc.Name; Arguments = args; GenericArguments = fc.GenericArguments}))

    (match unwrapped with
    | AssignmentExpression (e1, e2) -> 
        Result.map2 (fun t1 t2 -> AssignmentExpression(t1,t2)) (recurse e1) (recurse e2), inferExpression expr
    | BinaryExpression (e1, op, e2) -> 
        Result.map2 (fun e1 e2 -> BinaryExpression(e1,op,e2)) (recurse e1) (recurse e2), inferExpression expr
    | IdentifierExpression i -> Result.succeed (IdentifierExpression(i)), inferExpression expr
    | LiteralExpression l -> Result.succeed (LiteralExpression(l)), inferExpression expr
    | LocalFunctionCallExpression fc -> 
        (annotateFunction fc |> Result.map (fun fc -> LocalFunctionCallExpression(fc))), inferExpression expr
    | ListInitializerExpression l -> 
        let listMembers = l |> List.map recurse |> Result.merge
        (listMembers |> Result.map (fun listMembers -> ListInitializerExpression(listMembers))), 
            inferExpression expr
    | InstanceMemberExpression (callee, m) -> 
        (recurse callee,
            (match m with
            | MemberFunctionCall mfc ->
                (annotateFunction mfc)
                |> Result.map (fun mfc -> MemberFunctionCall(mfc))
            | MemberField f ->
                 Result.succeed (MemberField(f)))
        )
        ||> Result.map2 (fun c m -> InstanceMemberExpression(c, m)), inferExpression expr
    | NewExpression(t, args) -> 
        let args =  args |> List.map recurse |> Result.merge
        args |> Result.map (fun args -> NewExpression(t,args)), inferExpression expr
    | StaticMemberExpression (t, call) -> 
        (match call with
        | MemberFunctionCall mfc ->
            annotateFunction mfc
            |> Result.map (fun mfc -> MemberFunctionCall(mfc))
        | MemberField f ->
            Result.succeed (MemberField(f)) 
        )
        |> Result.map (fun m -> StaticMemberExpression(t, m))
            , inferExpression expr
    | UnaryExpression(op, e) -> (recurse e) |> Result.map (fun e -> UnaryExpression(op,e)), inferExpression expr
    )
    ||> Result.map2 (fun e t -> InferredTypeExpression(e,t))


let private lookupLocalVariable (variables : Map<string, TypeIdentifier>) name = 
    variables 
    |> Map.tryFind name
    |> function 
       | Some t ->  Result.succeed t
       | None -> Result.failure (UndefinedVariable name)

let rec private inferStatement 
    inferExpression
    (currentClass: Types.Type) 
    declaredVariables
    (statement : Statement<AstExpression>)
    : (CompilerResult<Statement<InferredTypeExpression>> * Map<string, TypeIdentifier>) = 
    let inferStatement = inferStatement inferExpression currentClass 
    let recurse = inferStatement declaredVariables
    let inferExpression = inferExpression (lookupLocalVariable declaredVariables)
    let annotate = annotate inferExpression
    let withOldVariables statement = statement, declaredVariables
    let withNewVariable (variableName, variableType) statement = 
        statement, declaredVariables |> Map.add variableName variableType

    match statement with
    | AssignmentStatement (e1, e2) ->
        Result.map2 (fun e1 e2 -> AssignmentStatement(e1, e2)) 
            (annotate e1) (annotate e2)
        |> withOldVariables
    | BreakStatement ->
        BreakStatement 
        |> Result.succeed 
        |> withOldVariables
    | CompositeStatement cs ->
        cs
        |> List.mapFold inferStatement declaredVariables
        |> fst
        |> Result.merge
        |> Result.map (CompositeStatement)
        |> withOldVariables
    | FunctionCallStatement fc ->
        fc.Arguments 
        |> List.map annotate 
        |> Result.merge
        |> Result.map 
            (fun args -> 
                FunctionCallStatement(
                    {
                     Name = fc.Name;
                     GenericArguments = fc.GenericArguments;
                     Arguments = args
                     }))
            |> withOldVariables
    | IfStatement (e, s, elseS) ->
        Result.map3 (fun e s elseS -> IfStatement(e, s,elseS))
            (annotate e) (recurse s |> fst) (elseS |> Result.mapOption (recurse >> fst))
            |> withOldVariables
    | InstanceMemberFunctionCallStatement (callee, fc) ->
        failwith "TODO:"
    | ReturnStatement exprOption ->
        exprOption
        |> Result.mapOption annotate
        |> Result.map (ReturnStatement)
        |> withOldVariables
    | StaticFunctionCallStatement (t,c) ->
        c.Arguments 
        |> List.map annotate 
        |> Result.merge
        |> Result.map 
            (fun args -> StaticFunctionCallStatement(t, {
                         Name = c.Name;
                         GenericArguments = c.GenericArguments;
                         Arguments = args
                     }))
            |> withOldVariables
    | ValueDeclaration(id, t, expr) ->
        let valueType = 
            if t.IsSome 
            then 
                Result.succeed (Identifier.typeId t.Value)
            else (inferExpression expr)
        Result.map (fun e -> 
            ValueDeclaration(id, t, e) ) (annotate expr)
            |> match valueType with
               | Success t -> withNewVariable(id, t)
               | Failure _ -> withOldVariables
    | VariableDeclaration vd ->
        match vd with
        | DeclarationWithInitialization (id, e) ->
            let valueType = inferExpression e
            Result.map 
                (fun e -> 
                    VariableDeclaration(DeclarationWithInitialization(id, e)) )
                (annotate e)
            |> match valueType with
                   | Success t -> withNewVariable(id, t)
                   | Failure _ -> withOldVariables
        | DeclarationWithType(id, t) ->
            VariableDeclaration(DeclarationWithType(id, t))
            |> Result.succeed
            |> withNewVariable (id, (Identifier.typeId t))
        | FullDeclaration (id, t, e) ->
            Result.map 
                (fun e -> VariableDeclaration(FullDeclaration(id, t, e)))
                (annotate e)
                |> withNewVariable (id, (Identifier.typeId t))
    | WhileStatement (e, s) -> 
        Result.map2 
            (fun e s -> WhileStatement(e,s) ) 
            (annotate e) 
            (recurse s |> fst)
            |> withOldVariables

let private leastUpperBoundIdentifier knownTypes ids =
    ids 
    |> List.map (fun id -> Map.find id knownTypes)
    |> leastUpperBound knownTypes
    |> (fun t -> t.Identifier)


let private inferFunction knownTypes leastUpperBound (currentType : Types.Type) (f : Ast.Function<AstExpression>) = 
    let inferExpression = inferExpression currentType knownTypes leastUpperBound 
    let inferStatement = inferStatement inferExpression currentType 
    let functionArguments =
        f.Parameters
        |> List.map (fun p -> (fst p, snd p |> Identifier.typeId))
        |> Map.ofList
    Result.map 
        (fun body -> {Body = body; ReturnType = f.ReturnType; Parameters = f.Parameters; Name = f.Name })
        (f.Body
            |> List.mapFold inferStatement functionArguments
            |> fst
            |> Result.merge
        )

let private inferConstructor knownTypes leastUpperBound (currentType : Types.Type) (c : Ast.Constructor<AstExpression>) = 
    let localFunctionType = findFunctionTypeInClass currentType
    let lookupLocalVariable = lookupLocalVariable (currentType.Fields |> List.map (fun f -> f.FieldName, f.Type) |> Map.ofList)
    let inferStatement = inferStatement (inferExpression currentType knownTypes leastUpperBound) currentType 
    let inferExpression = inferExpression currentType knownTypes leastUpperBound lookupLocalVariable
    let annotate = annotate inferExpression
    let args =
        c.Parameters
        |> List.map (fun p -> (fst p, snd p |> Identifier.typeId))
        |> Map.ofList
    Result.map2 
        (fun body args -> {Statements = body; Parameters = c.Parameters;  BaseClassConstructorCall = args})
        (c.Statements
            |> List.mapFold inferStatement args
            |> fst
            |> Result.merge
        )
        (c.BaseClassConstructorCall
            |> List.map (annotate)
            |> Result.merge
        )
let inferProperty knownTypes leastUpperBound (currentType : Types.Type) (p : Ast.Property<AstExpression>) =
    let lookupLocalVariable = lookupLocalVariable (currentType.Fields |> List.map (fun f -> f.FieldName, f.Type) |> Map.ofList)
    let inferExpression = inferExpression currentType knownTypes leastUpperBound lookupLocalVariable
    let annotate = annotate inferExpression
    Result.map
        (fun init -> {Name = p.Name; Initializer = init; Type = p.Type})
        (Result.mapOption annotate p.Initializer)

let private inferClass knownTypes (c : ModuleClass<AstExpression>) : CompilerResult<ModuleClass<InferredTypeExpression>>=
    let leastUpperBound = leastUpperBoundIdentifier knownTypes
    let currentType = (knownTypes |> Map.find (c.Identifier))
    let inferFunction = inferFunction knownTypes leastUpperBound currentType
    let inferConstructor = inferConstructor knownTypes leastUpperBound currentType
    let inferProperty = inferProperty knownTypes leastUpperBound currentType
    Result.map3
        (fun functions properties ctor -> 
        {
        Identifier = c.Identifier; 
        Functions = functions; 
        BaseClass = c.BaseClass; 
        ImplementedInterfaces = c.ImplementedInterfaces; 
        Properties = properties
        Constructor = ctor } )

        (c.Functions
        |> List.map inferFunction 
        |> Result.merge)
        (c.Properties
        |> List.map inferProperty
        |> Result.merge)
        (Result.mapOption inferConstructor c.Constructor)

let private inferModule knownTypes (modul : Module<AstExpression>) =
    let leastUpperBound = leastUpperBoundIdentifier knownTypes
    let inferFunction = inferFunction knownTypes leastUpperBound (knownTypes |> Map.find modul.Identifier)
    Result.map2
        (fun functions classes -> { Identifier = modul.Identifier; Functions = functions; Classes = classes } )
        (modul.Functions
        |> List.map inferFunction
        |> Result.merge)
        (modul.Classes
        |> List.map (inferClass knownTypes)
        |> Result.merge)


let inferTypes (modules : Module<AstExpression> list, knownTypes : Map<TypeIdentifier, Type>) = 
    modules
    |> List.map (inferModule knownTypes)
    |> Result.merge
