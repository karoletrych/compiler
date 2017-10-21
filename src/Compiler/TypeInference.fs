module Compiler.TypeInference

open Compiler.Ast
open Compiler.Types

let leastUpperBound (knownTypes : Map<string, Type>) types=
    let rec allAncestors (t : Type) : Type list  =
        match t.BaseTypes with 
        | [] -> [] 
        | b -> (b |> (List.map (fun x -> knownTypes.[x])))
               @ (b |> List.collect (fun x -> (allAncestors knownTypes.[x])))

    let children allNodes parentNode = 
                allNodes 
                |> List.filter (fun (t : Type) -> 
                    t.BaseTypes |> List.contains parentNode
                    )

    let rec longestPath (root : Type) allNodes =
        let childrenOf = children allNodes
        let isALeaf = childrenOf root.Name = List.empty
        if isALeaf 
        then
            ([root], 0)
        else
            let edges = childrenOf root.Name |> List.map (fun child ->
                let (p, l) = longestPath child allNodes
                (child :: p, l + 1)
                )
            edges |> List.maxBy snd

    let longest = longestPath knownTypes.["System.Object"]

    types
        |> List.map (fun t -> (t::allAncestors t))
        |> List.map Set.ofList
        |> Set.intersectMany
        |> Set.toList
        |> List.distinct
        |> longest
        |> fst
        |> List.last

// TODO: split into function composition
let inferTypes (knownTypes : Map<string, Type>) ast =
    let lookupPreviouslyDeclaredType identifier declaredVariables : Types.Type =
        failwith "not implemented" 
    let lookupDeclaredFunctions identifier : Types.Type =
        failwith "not implemented" 
    let lookupMemberFunctions identifier : Types.Type =
        failwith "not implemented"
    let inferBinaryExpressionType e1 op e2 = 
        failwith "not implemented"
    let lookupStaticFunctionReturnType staticCall =
        failwith "not implemented"

    let typeOfLiteral = 
        function
        | BoolLiteral _-> knownTypes.["System.Boolean"];
        | IntLiteral _-> knownTypes.["System.Int32"];
        | FloatLiteral _-> knownTypes.["System.Single"];
        | StringLiteral _-> knownTypes.["System.String"];

    let rec inferExpressionType 
        (declaredVariables : Map<Ast.Identifier, Types.Type>) 
        (expression : Ast.Expression) 
            : Types.Type =
        let leastUpperBound = leastUpperBound knownTypes

        let lookupType = lookupPreviouslyDeclaredType declaredVariables
        let inferExpressionType = inferExpressionType declaredVariables
        let lookupListType =
                List.map inferExpressionType 
                >> leastUpperBound
        // if List.forall (fun (t : Type option) -> t.IsSome) types 
        // then Some (leastUpperBound (types |> List.map (fun x-> x.Value))) 
        // else None

        match expression with
        | AssignmentExpression(_, e) -> inferExpressionType e
        | BinaryExpression(e1, op, e2) -> inferBinaryExpressionType (inferExpressionType e1) op (inferExpressionType e2)
        | ExpressionWithInferredType _ -> failwith "TODO:"
        | FunctionCallExpression(fc) -> lookupDeclaredFunctions fc
        | IdentifierExpression(ie) -> lookupType ie 
        | LiteralExpression(le) -> typeOfLiteral le
        | ListInitializerExpression list -> lookupListType list
        | MemberExpression mfc -> lookupMemberFunctions mfc
        | NewExpression (t, _) -> lookupType (t.ToString())
        | StaticMemberExpression (t, call) -> lookupStaticFunctionReturnType (t,call)
        | UnaryExpression(_, e) -> inferExpressionType e

    let annotateExpression variables e =
        let exprType = inferExpressionType variables e
        ExpressionWithInferredType(e, exprType.Name)


    let rec annotateStatement 
        (declaredVariables : Map<Identifier, Type>) 
        (statement : Ast.Statement)
        : (Statement * Map<Identifier, Type>) = 
        let annotateExpression = annotateExpression declaredVariables
        
        match statement with
        | AssignmentStatement (e1, e2) ->
            AssignmentStatement(annotateExpression e1, annotateExpression e2), declaredVariables
        | BreakStatement ->
            BreakStatement, declaredVariables
        | CompositeStatement cs ->
            (cs 
            |> List.map ((fun s -> annotateStatement declaredVariables s) >> fst)
            |> CompositeStatement) , declaredVariables
        | FunctionCallStatement fcs ->
            FunctionCallStatement fcs, declaredVariables
        | IfStatement (e, s, elseS) ->
            IfStatement(e,
             annotateStatement declaredVariables s |> fst,
              elseS |> Option.map (fun elseS -> annotateStatement declaredVariables elseS |> fst) )
              ,
               declaredVariables
        | MemberFunctionCallStatement mfcs ->
            MemberFunctionCallStatement mfcs, declaredVariables
        | ReturnStatement exprOption ->
            exprOption
            |> Option.map annotateExpression
            |> ReturnStatement, declaredVariables // TODO: ambiguous. annotateExpression should return Result<T> 
        | StaticFunctionCallStatement (t,c) ->
            StaticFunctionCallStatement (t,c), declaredVariables
        | ValueDeclaration(id, t, expr) ->
            ValueDeclaration(id, t, annotateExpression expr),
            let valueType = 
                        t 
                        |> Option.map (fun t -> t.ToString())
                        |> Option.defaultValue (inferExpressionType declaredVariables expr).Name
                        |> fun x -> knownTypes.[x]
            declaredVariables |> Map.add id valueType
        | VariableDeclaration vd ->
            (
            match vd with
            | DeclarationWithInitialization (id, e) ->
                VariableDeclaration(DeclarationWithInitialization(id, annotateExpression e)),
                 declaredVariables |> Map.add id (inferExpressionType declaredVariables e)
            | DeclarationWithType(id, t) ->
                VariableDeclaration(DeclarationWithType(id, t)),
                declaredVariables |> Map.add id knownTypes.[t.ToString()] // TODO: potential exception (?)
            | FullDeclaration (id, t, e) ->
                VariableDeclaration(FullDeclaration(id, t, annotateExpression e)),
                declaredVariables |> Map.add id knownTypes.[t.ToString()] // TODO: ^ 
            )
        | WhileStatement (e,s) -> 
            WhileStatement(annotateExpression e, s), declaredVariables
    let inferFunction (f: Ast.Function) = 
        let declaredVariables =
            f.Parameters
            |> Map.ofList
        f.Body
        |> List.mapFold annotateStatement Map.empty<string, Type>
        |> fst

    ast 
    |> List.map
    (function
    | FunctionDeclaration f -> FunctionDeclaration {f with Body = inferFunction f}
    | ClassDeclaration c -> 
        ClassDeclaration {
            c with
                FunctionDeclarations =
                    c.FunctionDeclarations |> List.map (fun f -> {f with Body = inferFunction f})
            })
        

    