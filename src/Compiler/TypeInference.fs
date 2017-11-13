module Compiler.TypeInference

open Compiler.Ast
open Types
open CompilerResult
open AstProcessing

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

let private inferExpression leastUpperBound localFunction lookupLocalVariable types expression =
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
        |> Result.bind (fun argTypes -> localFunction (name, argTypes, generics))
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
    let memberExpression (e1, e2) = failwith "TODO:"
    let newExpression (t, _) = Identifier.lookupType types t
    let staticMember (t, (name,args,generics)) = failwith "TODO:"

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

let rec private annotateStatement 
    (inferExpression : (Map<string, TypeIdentifier> -> Expression -> CompilerResult<TypeIdentifier>))
    (currentClass: Types.Type) 
    (declaredVariables : Map<string, TypeIdentifier>) 
    (statement : Ast.Statement)
    : (CompilerResult<Statement> * Map<string, TypeIdentifier>) = 
    let annotateStatement = annotateStatement inferExpression currentClass 
    let recurse = annotateStatement declaredVariables
    let inferExpression = inferExpression declaredVariables
    let annotate e =
        inferExpression e
        |> Result.map (fun t -> InferredTypeExpression(e, t))
    let withOldVariables statement = statement, declaredVariables
    let withNewVariable (variableName, variableType) statement = 
        statement, declaredVariables |> Map.add variableName variableType

    let typeId t = 
        let (TypeIdentifier ti) = t
        ti
    
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
        |> List.mapFold annotateStatement declaredVariables
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
                Result.succeed (typeId t.Value)
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
            |> withNewVariable (id, (typeId t))
        | FullDeclaration (id, t, e) ->
            Result.map 
                (fun e -> VariableDeclaration(FullDeclaration(id, t, e)))
                (annotate e)
                |> withNewVariable (id, (typeId t))
    | WhileStatement (e, s) -> 
        Result.map2 
            (fun e s -> WhileStatement(e,s) ) 
            (annotate e) 
            (recurse s |> fst)
            |> withOldVariables


// TODO: split into function composition
let inferTypes (modul : Module, knownTypes : Map<TypeIdentifier, Type>) = 
    let inferFunction (c : Ast.Class) (f: Ast.Function) = 
        let annotateStatement = annotateStatement c
        let declaredVariables =
            f.Parameters
            |> List.map (fun p -> (fst p, knownTypes.[(snd p) |> Identifier.fromTypeSpec]))
            |> Map.ofList
        f.Body
        |> List.mapFold annotateStatement declaredVariables
        |> fst

    modul.Classes
    |> List.map (fun c -> 
        {
            c with
                FunctionDeclarations =
                    c.FunctionDeclarations 
                    |> List.map (fun f -> {f with Body = inferFunction c f})
        })
    |> fun c -> Result.succeed {Classes = c}
        
