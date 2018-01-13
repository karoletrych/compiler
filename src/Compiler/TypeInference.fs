module Compiler.TypeInference

open Compiler.Ast
open Types
open CompilerResult
open AstProcessing

type InferredTypeExpression = InferredTypeExpression of Expression<InferredTypeExpression> * TypeIdentifier

/// gets inferred type of expression
let getType expr = 
    let (InferredTypeExpression(_, t)) = expr
    t
/// gets expression from InferredTypeExpression
let getExpression expr = 
    let (InferredTypeExpression(e, _)) = expr
    e

/// gets internal expression from AstExpression
let unwrapExpression expr = 
    let (AstExpression expr) = expr
    expr

/// gets Types.Type of typespec which represents builtin type
let builtInType (types : Map<TypeIdentifier, Type>) (typeSpec : TypeSpec) =
    types.[typeSpec |> Identifier.fromTypeSpec]

/// assumes type is not a generic type or parameter. return the type.
let getConstructedType field =
    match field.TypeRef with
          | ConstructedType t -> (field.FieldName, t)
          | GenericTypeDefinition(_) -> failwith "no support for generic type definition"
          | GenericParameter _-> failwith "no support for generic parameters"


/// finds least upper bound of given Types.Type instances
let private leastUpperBound (knownTypes : Map<TypeIdentifier, Type>) types =
    let rec allAncestors (t : Type) : Type list  =
        match t.BaseTypes with 
        | [] -> [] 
        | b -> b @ (b |> List.collect allAncestors)

    let children allNodes parentNode = 
                allNodes 
                |> List.filter (fun (t : Type) -> 
                    t.BaseTypes |> List.contains parentNode
                    )

    /// finds longest path from root to any leaf and returns it and its length
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
        |> List.map ((fun t -> t::allAncestors t) >> Set.ofList)
        |> Set.intersectMany // intersection of paths to root
        |> Set.toList
        |> List.distinct  // remove duplicate nodes
        |> longest
        |> fst   // path
        |> List.last // furthest element from root




let private findFunction (t, (name, args : TypeIdentifier list, generics), isStatic) = 
        t.Methods 
        |> List.tryFind 
            (fun f -> f.Name = name
                     // && f.Parameters |> List.map (fun p -> p.Type) = args TODO:
                     && if not (List.isEmpty generics) 
                        then List.length f.GenericParameters = List.length generics 
                        else true
                        && f.IsStatic = isStatic)

let private findFunctionTypeInClass t ((name, args : TypeIdentifier list, generics), isStatic) : CompilerResult<TypeRef> =
    let matchingFunction = findFunction (t, (name, args, generics), isStatic)
    match matchingFunction with
    | Some f -> 
        match f.ReturnType with
        | Some ret -> Result.succeed ret
        | None -> Result.failure (ReturnTypeNeedsToBeSpecified( t.Identifier, name, args, generics))
    | None -> Result.failure (FunctionNotFound(t.Identifier, name, args, generics))

let private findLocalFunctionType otherLocalFunctions ((name, args), _) : CompilerResult<TypeIdentifier> =
    match otherLocalFunctions |> Map.tryFind (name,args) with
    | Some ret -> Result.succeed ret
    | None -> Result.failure (FunctionTypeCannotBeInferred(name, args))

let getGenericArgument types number genericParameter calleeType = 
    match genericParameter with
    | DeclaredInParameterizedType ->
        let matchedType = types |> List.find (fun v -> Identifier.equalWithoutGeneric v calleeType)
        let (GenericArgument arg) = matchedType.GenericParameters.[number]
        arg
    | DeclaredInOtherType t ->
        let matchedType = types |> List.find (fun v -> Identifier.equalWithoutGeneric v t)
        let (GenericArgument arg) = matchedType.GenericParameters.[number]
        arg
    | GenericArgument t ->
        failwith "unexpected"

let rec bindGenericParameters (boundDeclaringTypes : List<TypeIdentifier>) unboundTypeId calleeType : TypeIdentifier =
    {
        unboundTypeId with
            GenericParameters = 
                unboundTypeId.GenericParameters 
                |> List.mapi 
                    (fun i genericParameter -> 
                        GenericArgument (getGenericArgument boundDeclaringTypes i genericParameter calleeType))
    }

let getReturnType (calleeType : TypeIdentifier) returnTRef =
    match returnTRef with
    | ConstructedType(t) -> 
        Result.succeed t
    | GenericParameter (declarationPlace, position) ->
        match declarationPlace with
        | Class ->
            Result.succeed 
                (
                match calleeType.GenericParameters.[position] with
                | GenericArgument tArg -> tArg
                | _ -> failwith "not expected")
        | Method -> Result.failure (NotSupported "Usage of generic methods is not supported yet")
    | GenericTypeDefinition unboundT ->
        let rec substituteGenericArgsIdentifier unboundT =
            let rec allEnclosingTypes t =
                t :: 
                    match t.DeclaringType with 
                    | Some dt -> allEnclosingTypes dt
                    | None -> []
            let boundEnclosingTypes = allEnclosingTypes calleeType    
            bindGenericParameters boundEnclosingTypes unboundT calleeType
        let gtd = substituteGenericArgsIdentifier unboundT
        let gtd = gtd
        Result.succeed gtd
        
 
let private findFieldTypeInClass knownTypes calleeType (name, isStatic) = 
    let t = 
        knownTypes 
        |> Map.pick (fun tId t -> if Identifier.equalWithoutGeneric tId calleeType then Some t else None)
    let fieldOption = 
        t.Fields 
        |> List.tryFind (fun f -> f.FieldName = name && f.IsStatic = isStatic)
    match fieldOption with
    | Some field -> getReturnType calleeType field.TypeRef 
    | None -> Result.failure (FieldNotFound(calleeType, name))

let typeOfLiteral = 
    (
    function
    | BoolLiteral _-> Identifier.bool
    | IntLiteral _-> Identifier.int
    | FloatLiteral _-> Identifier.float
    | StringLiteral _-> Identifier.string
    ) >> Result.succeed

let private inferExpression localFunctions (currentClass : Types.Type) (knownTypes : Map<TypeIdentifier, Types.Type>) leastUpperBound lookupLocalVariable expression =
    let instanceMemberFunctionCall (knownTypes : Map<TypeIdentifier, Type>) (callee, (name, args, generics)) : CompilerResult<TypeIdentifier>= 
        (callee, args |> Result.merge) 
        ||> Result.bind2 
            (fun calleeTypeId args -> 
                let unboundCalleeType = 
                    knownTypes 
                    |> Map.pick (fun tId t -> 
                        if Identifier.equalWithoutGeneric tId calleeTypeId 
                        then Some t 
                        else None)
                let returnTRef = findFunctionTypeInClass unboundCalleeType ((name,args,generics), false)
                returnTRef |> Result.bind (getReturnType calleeTypeId)
                )
    let assignment ((t1, i), _) = 
        t1
        |> Result.bind (fun calleeType -> 
            findFieldTypeInClass knownTypes calleeType (i, false))
    let binary (t1, op, t2) =
        let commonType t1 t2 = 
            if t1 = t2 
            then Result.succeed t1 
            else Result.failure 
                    (CannotInferBinaryExpressionType (t1, t2))
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
    let localFunctionCall (name, args, _) = 
        args 
        |> Result.merge 
        |> Result.bind (fun argTypes -> findLocalFunctionType localFunctions ((name, argTypes), currentClass.IsStatic))
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
        |> Result.map Identifier.listOf
    let memberField (callee, f) = 
        callee 
        |> Result.bind (fun calleeType -> findFieldTypeInClass knownTypes calleeType (f, false))
    let newExpression (t, _) = 
        Result.succeed (Identifier.typeId t)
    let staticMemberFunctionCall (t, (name, args, generics)) = 
        args 
        |> Result.merge
        |> Result.bind 
            (fun args -> findFunctionTypeInClass 
                            (knownTypes.[Identifier.typeId t]) ((name, args, generics), true))
        |> Result.map (fun t ->
                            match t with
                            | ConstructedType t -> t
                            | GenericTypeDefinition(_) -> failwith "Not Implemented" 
                            | GenericParameter _ -> failwith "not supported")
    let staticMemberField (t, f) = 
        findFieldTypeInClass knownTypes (Identifier.typeId t)  (f, true)

    let unary (_, t) = t
    expressionCata 
        assignment
        binary 
        localFunctionCall 
        identifier 
        literal 
        listInitializer 
        (instanceMemberFunctionCall knownTypes)
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
    | AssignmentExpression (assignee, e2) -> 
        match assignee with
        | MemberFieldAssignee (e1, i) ->
            Result.map2 (fun t1 t2 -> AssignmentExpression(MemberFieldAssignee(t1,i),t2)) (recurse e1) (recurse e2), inferExpression expr
        | IdentifierAssignee i ->
            Result.map (fun t2 -> AssignmentExpression(IdentifierAssignee(i),t2)) (recurse e2), inferExpression expr
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
       | None -> Result.failure (UndefinedVariableOrField name)

let rec private inferStatement 
    (knownTypes : Map<TypeIdentifier, Type>)
    inferExpression
    (currentClass: Types.Type) 
    declaredVariables
    (statement : Statement<AstExpression>)
    : (CompilerResult<Statement<InferredTypeExpression>> * Map<string, TypeIdentifier>) = 
    let inferStatement = inferStatement knownTypes inferExpression currentClass 
    let recurse = inferStatement declaredVariables
    let inferExpression = inferExpression (lookupLocalVariable declaredVariables)
    let annotate = annotate inferExpression
    let withOldVariables statement = statement, declaredVariables
    let withNewVariable (variableName, variableType) statement = 
        statement, declaredVariables |> Map.add variableName variableType

    match statement with
    | AssignmentStatement (assignee, e2) ->
        match assignee with
        | MemberFieldAssignee (e1, i) ->
            Result.map2 (fun e1 e2 -> AssignmentStatement(MemberFieldAssignee(e1,i), e2)) 
                (annotate e1) (annotate e2)
        | IdentifierAssignee i ->
            Result.map (fun e -> AssignmentStatement(IdentifierAssignee(i), e)) 
                (annotate e2) 
        |> withOldVariables
    | CompositeStatement cs ->
        cs
        |> List.mapFold inferStatement declaredVariables
        |> fst
        |> Result.merge
        |> Result.map (CompositeStatement)
        |> withOldVariables
    | LocalFunctionCallStatement fc ->
        fc.Arguments 
        |> List.map annotate 
        |> Result.merge
        |> Result.map 
            (fun args -> 
                LocalFunctionCallStatement(
                    {
                     Name = fc.Name;
                     GenericArguments = fc.GenericArguments;
                     Arguments = args
                    }))
            |> withOldVariables
    | IfStatement (e, s, elseS) ->
        Result.map3 (fun e s elseS -> IfStatement(e, s, elseS))
            (annotate e) (recurse s |> fst) (elseS |> Result.mapOption (recurse >> fst))
            |> withOldVariables
    | InstanceMemberFunctionCallStatement (callee, fc) ->
        let statement = 
            (annotate callee, fc.Arguments |> List.map annotate |> Result.merge)
            ||> Result.map2(fun callee args ->
                    InstanceMemberFunctionCallStatement( 
                            (callee, {
                                     Name = fc.Name;
                                     GenericArguments = fc.GenericArguments;
                                     Arguments = args
                                    })))
        statement |> withOldVariables
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
            else inferExpression expr
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
                    VariableDeclaration(DeclarationWithInitialization(id, e)))
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

let private inferReturnType leastUpperBound body =
    let addReturn acc ret = 
        match ret with 
        | Some r -> r :: acc
        | None -> acc
    let idFold acc _ = acc
    let ifStatement (s, elseS) =
        s @ (elseS |> Option.defaultValue [])

    statementFold
        idFold idFold idFold idFold idFold idFold addReturn idFold idFold idFold idFold ifStatement
            [] (CompositeStatement body) 
    |> List.map getType
    |> function
       | [] -> Identifier.``void``
       | types -> leastUpperBound types
    |> TypeIdentifier

let private inferFunction knownTypes leastUpperBound (currentType : Types.Type) (localFunctionTypes : Map<string*TypeIdentifier list,TypeIdentifier>) (f : Ast.Function<AstExpression>) = 
    let inferExpression = inferExpression localFunctionTypes currentType knownTypes leastUpperBound 
    let inferStatement = inferStatement knownTypes inferExpression currentType 
    let identifiers =
        f.Parameters
        |> List.map (fun p -> (fst p, snd p |> Identifier.typeId))
        |> List.append (currentType.Fields |> List.map getConstructedType)
        |> Map.ofList

    let result = 
        (f.Body
            |> List.mapFold inferStatement identifiers
            |> fst
            |> Result.merge)
        |> Result.map
            (fun body -> 
                {
                    Body = body
                    ReturnType = 
                        Some (f.ReturnType |> Option.defaultWith (fun () -> inferReturnType leastUpperBound body))
                    Parameters = f.Parameters
                    Name = f.Name
                })
    match result with
    | Failure _ -> result, localFunctionTypes
    | Success f -> result, localFunctionTypes 
                           |> Map.add (f.Name, f.Parameters |> List.map (snd >> Identifier.typeId)) (f.ReturnType.Value |> Identifier.typeId)
        

let private inferConstructor knownTypes leastUpperBound (currentType : Types.Type) (c : Ast.Constructor<AstExpression>) = 
    let lookupLocalVariable = 
        lookupLocalVariable 
            (currentType.Fields 
            |> List.map getConstructedType |> Map.ofList)
    let inferStatement = inferStatement knownTypes (inferExpression Map.empty currentType knownTypes leastUpperBound) currentType 
    let inferExpression = inferExpression Map.empty currentType knownTypes leastUpperBound lookupLocalVariable
    let annotate = annotate inferExpression
    let args =
        (c.Parameters |> List.map (fun p -> (fst p, snd p |> Identifier.typeId)))
      @ (currentType.Fields |> List.map getConstructedType)
        |> Map.ofList
    Result.map2 
        (fun body args -> { Body = body; Parameters = c.Parameters; BaseClassConstructorCall = args})
        (c.Body
            |> List.mapFold inferStatement args
            |> fst
            |> Result.merge
        )
        (c.BaseClassConstructorCall
            |> List.map (annotate)
            |> Result.merge
        )
let inferField knownTypes leastUpperBound (currentType) (field : Ast.Field<AstExpression>) =
    let lookupLocalVariable = 
        lookupLocalVariable 
            (currentType.Fields 
                |> List.map getConstructedType
                |> Map.ofList)
    let inferExpression = inferExpression Map.empty currentType knownTypes leastUpperBound lookupLocalVariable
    let annotate = annotate inferExpression
    Result.map
        (fun init -> {Name = field.Name; Initializer = init; Type = field.Type; IsReadOnly = field.IsReadOnly})
        (Result.mapOption annotate field.Initializer)

let private inferClass knownTypes (c : ModuleClass<AstExpression>) : CompilerResult<ModuleClass<InferredTypeExpression>>=
    let leastUpperBound = leastUpperBoundIdentifier knownTypes
    let currentType = (knownTypes |> Map.find c.Identifier)
    let inferFunction = inferFunction knownTypes leastUpperBound currentType
    let inferConstructor = inferConstructor knownTypes leastUpperBound currentType
    let inferField = inferField knownTypes leastUpperBound currentType
    Result.map3
        (fun functions fields ctor -> 
        {
            Identifier = c.Identifier; 
            Functions = functions; 
            BaseClass = c.BaseClass; 
            Fields = fields
            Constructors = ctor 
        } )

        (c.Functions
        |> List.mapFold inferFunction Map.empty
        |> fst
        |> Result.merge)
        (c.Fields
        |> List.map inferField
        |> Result.merge)
        (c.Constructors |> List.map inferConstructor |> Result.merge)

let private inferModule knownTypes (modul : Module<AstExpression>) =
    let leastUpperBound = leastUpperBoundIdentifier knownTypes
    let inferFunction = inferFunction knownTypes leastUpperBound (knownTypes |> Map.find modul.Identifier)
    (
        (modul.Functions
        |> List.mapFold inferFunction Map.empty
        |> fst
        |> Result.merge),
        (modul.Classes
        |> List.map (inferClass knownTypes)
        |> Result.merge)
    )
    ||> Result.map2
            (fun functions classes -> { Identifier = modul.Identifier; Functions = functions; Classes = classes } )

let inferTypes (modules : Module<AstExpression> list, knownTypes : Map<TypeIdentifier, Type>) = 
    modules
    |> List.map (inferModule knownTypes)
    |> Result.merge
    |> Result.map (fun modules -> modules, knownTypes)
