module Compiler.TypeInference

open Compiler.Ast
open Compiler.Types

let builtInType (types : Map<string, Type>) (t : TypeSpec) =
    types.[t.ToString()]

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

    let longest = longestPath (builtInType knownTypes Object)

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
let inferTypes (knownTypes : Map<string, Type>) (ast : Declaration list) : Declaration list= 
    let lookupLocalVariable (declaredVariables : Map<Identifier, Type>) identifier: Types.Type option =
        Some declaredVariables.[identifier]
    let lookupDeclaredFunctions identifier : Types.Type option =
        failwith "not implemented" 
    let lookupMemberFunctions identifier : Types.Type option =
        failwith "not implemented"
    let inferBinaryExpressionType expr1Type op expr2Type = 
        let someIfEqual e1 e2 = if e1 = e2 then Some e1 else None
        match op,expr1Type,expr2Type with
        | _, None, _ -> None
        | _, _, None -> None
        | Plus, Some e1, Some e2 -> someIfEqual e1 e2
        | Minus, Some e1, Some e2 -> someIfEqual e1 e2
        | Multiplication, Some e1, Some e2 -> someIfEqual e1 e2
        | Division, Some e1, Some e2 -> someIfEqual  e1 e2
        | Remainder, Some e1, Some e2 -> someIfEqual e1 e2
        | _ -> Some (builtInType knownTypes Bool)
    let lookupStaticFunctionReturnType staticCall =
        failwith "not implemented"

    let typeOfLiteral = 
        (function
        | BoolLiteral _-> builtInType knownTypes Bool;
        | IntLiteral _-> builtInType knownTypes Int;
        | FloatLiteral _-> builtInType knownTypes Float;
        | StringLiteral _-> builtInType knownTypes String;)
        >> Some

    let rec inferExpressionType 
        (declaredVariables : Map<Ast.Identifier, Types.Type>) 
        (expression : Ast.Expression) 
            : Types.Type option =
        let leastUpperBound = leastUpperBound knownTypes

        let lookupType = lookupLocalVariable declaredVariables
        let inferExpressionType = inferExpressionType declaredVariables
        let lookupListType types =
            if List.forall (fun (t : Type option) -> t.IsSome) types 
            then Some (leastUpperBound (types |> List.map (fun x-> x.Value))) 
            else None

        match expression with
        | AssignmentExpression(_, e) -> inferExpressionType e
        | BinaryExpression(e1, op, e2) ->
            inferBinaryExpressionType (inferExpressionType e1) op (inferExpressionType e2)
        | ExpressionWithInferredType _ -> failwith "TODO:"
        | FunctionCallExpression(fc) -> lookupDeclaredFunctions fc
        | IdentifierExpression(ie) -> lookupType ie 
        | LiteralExpression(le) -> typeOfLiteral le
        | ListInitializerExpression list -> lookupListType (list |> List.map inferExpressionType)
        | MemberExpression mfc -> lookupMemberFunctions mfc
        | NewExpression (t, _) -> lookupType (t.ToString())
        | StaticMemberExpression (t, call) -> lookupStaticFunctionReturnType (t,call)
        | UnaryExpression(_, e) -> inferExpressionType e

    let annotateExpression variables e =
        let exprType = inferExpressionType variables e
        ExpressionWithInferredType(e, exprType |> Option.map (fun e -> e.Name))


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
            |> ReturnStatement, declaredVariables
        | StaticFunctionCallStatement (t,c) ->
            StaticFunctionCallStatement (t,c), declaredVariables
        | ValueDeclaration(id, t, expr) ->
            ValueDeclaration(id, t, annotateExpression expr),

            let valueType = 
                        t 
                        |> Option.map (fun t -> knownTypes.[t.ToString()])
                        |> Option.orElse (
                             (inferExpressionType declaredVariables expr) |> Option.map (fun t -> knownTypes.[t.Name]))
            match valueType with
            | Some t -> declaredVariables |> Map.add id t
            | None -> declaredVariables
        | VariableDeclaration vd ->
            (
            match vd with
            | DeclarationWithInitialization (id, e) ->
                VariableDeclaration(DeclarationWithInitialization(id, annotateExpression e)),
                match inferExpressionType declaredVariables e with
                | Some t -> declaredVariables |> Map.add id t
                | None -> declaredVariables 
            | DeclarationWithType(id, t) ->
                VariableDeclaration(DeclarationWithType(id, t)),
                declaredVariables |> Map.add id knownTypes.[t.ToString()] // TODO: exception (?)
            | FullDeclaration (id, t, e) ->
                VariableDeclaration(FullDeclaration(id, t, annotateExpression e)),
                declaredVariables |> Map.add id knownTypes.[t.ToString()] // TODO: ^ 
            )
        | WhileStatement (e,s) -> 
            WhileStatement(annotateExpression e, s), declaredVariables
    let inferFunction (f: Ast.Function) = 
        let declaredVariables =
            f.Parameters
            |> List.map (fun p -> (fst p, knownTypes.[(snd p).ToString()]))
            |> Map.ofList
        f.Body
        |> List.mapFold annotateStatement Map.empty<string, Type>
        |> fst

    ast 
    |> List.map (fun decl ->
    match decl with
    | FunctionDeclaration f -> FunctionDeclaration {f with Body = inferFunction f}
    | ClassDeclaration c -> 
        ClassDeclaration {
            c with
                FunctionDeclarations =
                    c.FunctionDeclarations |> List.map (fun f -> {f with Body = inferFunction f})
            })
        
