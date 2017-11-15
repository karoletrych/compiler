module Compiler.TypeInference

open Compiler.Ast
open Types
open CompilerResult
open AstProcessing
open System.Drawing
open FSharpx.Collections.Heap

// TODO: consider: type Inferred = Inferred of Expression * TypeIdentifier option
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
let private localFunctionType (currentType : Type) (name, args, generics) : CompilerResult<TypeIdentifier> =
    let matchingFunction = 
        currentType.Methods
        |> List.tryFind 
            (fun f -> f.Name = name
                    && (f.Parameters |> List.map (fun p -> p.Type)) = args
                    //TODO: && (f.GenericParametersCount |> List.length = generics |> List.length)
                    )
    match matchingFunction with
    | Some f -> match f.ReturnType with
                | Some ret -> Result.succeed ret
                | None -> failwith "TODO: "
    | None -> Result.failure (FunctionNotFound(name,args,generics))

let typeOfLiteral = 
    (function
    | BoolLiteral _-> Identifier.bool
    | IntLiteral _-> Identifier.int
    | FloatLiteral _-> Identifier.float
    | StringLiteral _-> Identifier.string
    ) >> Result.succeed




let private inferExpression leastUpperBound localFunction lookupLocalVariable expression =
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
    let inferredType t = failwith "unexpected"
    let functionCall (name, args, generics) = 
        args 
        |> Result.merge 
        |> Result.bind (fun argTypes -> localFunction (name, argTypes, generics |> List.map Identifier.typeId))
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
    let memberExpression (e1, e2) = failwith "TODO:"
    let newExpression (t, _) = Result.succeed (Identifier.typeId t)
    let staticMember (t, (name, args, generics)) = failwith "TODO:"

    let unary (_, t) = t
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
    statement
    : (CompilerResult<Statement> * Map<string, TypeIdentifier>) = 
    let inferStatement = inferStatement inferExpression currentClass 
    let recurse = inferStatement declaredVariables
    let inferExpression = inferExpression (lookupLocalVariable declaredVariables)
    let annotate e =
        inferExpression e
        |> Result.map (fun t -> InferredTypeExpression(e, t))
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
    | FunctionCallStatement fcs ->
        fcs.Arguments 
        |> List.map annotate 
        |> Result.merge
        |> Result.map 
            (fun args -> FunctionCallStatement({fcs with Arguments = args}))
            |> withOldVariables
    | IfStatement (e, s, elseS) ->
        Result.map3 (fun e s elseS -> IfStatement(e, s,elseS))
            (annotate e) (recurse s |> fst) (elseS |> Result.mapOption (recurse >> fst))
            |> withOldVariables
    | MemberFunctionCallStatement mfcs ->
        failwith "TODO:"
    | ReturnStatement exprOption ->
        exprOption
        |> Result.mapOption annotate
        |> Result.map (ReturnStatement)
        |> withOldVariables
    | StaticFunctionCallStatement (t,c) ->
        failwith "TODO:"
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


let private inferFunction leastUpperBound (currentType : Types.Type) (f: Ast.Function) = 
    let localFunctionType = localFunctionType currentType
    let inferExpression = inferExpression leastUpperBound localFunctionType 
    let inferStatement = inferStatement inferExpression currentType 
    let functionArguments =
        f.Parameters
        |> List.map (fun p -> (fst p, (snd p) |> Identifier.typeId))
        |> Map.ofList
    Result.map 
        (fun body -> {f with Body = body})
        (f.Body
            |> List.mapFold inferStatement functionArguments
            |> fst
            |> Result.merge
        )

let inferTypes (modul : Module, knownTypes : Map<TypeIdentifier, Type>) = 
    modul.Classes
    |> List.map (fun c -> 
            let leastUpperBound = leastUpperBoundIdentifier knownTypes
            let inferFunction = inferFunction leastUpperBound (knownTypes |> Map.find (Identifier.fromClassDeclaration c))
            Result.map
                (fun functions -> {c with FunctionDeclarations = functions} )
                (c.FunctionDeclarations
                |> List.map inferFunction 
                |> Result.merge)
        )
    |> Result.merge 
    |> Result.map (fun c -> {Classes = c})
        
