module Compiler.TypeInference

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
        if isALeaf then
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

let lookupPreviouslyDeclaredType identifier declaredVariables : Types.Type option =
    None 

let lookupDeclaredFunctions identifier : Types.Type option =
    failwith "notimplemented" 
let inferBinaryExpressionType e1 op e2 = 
    failwith "not implemented"
let inferUnaryExpressionType op e = 
    failwith "not implemented"

let rec inferExpressionType 
    (expression : Ast.Expression) 
    (declaredVariables : Map<Ast.Identifier, Types.Type>) 
        : Types.Type option =
    let lookupType = lookupPreviouslyDeclaredType declaredVariables

    match expression with
        | Ast.IdentifierExpression(ie) -> lookupType ie 
        | Ast.FunctionCallExpression(fc) -> lookupDeclaredFunctions fc
        | Ast.LiteralExpression(le) -> Some (Types.typeOfLiteral le)
        | Ast.BinaryExpression(e1, op, e2) -> inferBinaryExpressionType (inferExpressionType e1) op (inferExpressionType e2)
        | Ast.UnaryExpression(op, e) -> inferUnaryExpressionType op (inferExpressionType e)
        | Ast.AssignmentExpression(id, e) -> inferExpressionType e declaredVariables

let resolveType t = 
    Some (Types.createBasicType (t.ToString()))

let createAnnotatedExpr e typ = {Expression = e; Type = typ}
let processStatement 
    (statement : Ast.Statement)
    (declaredVariables : Map<Ast.Identifier, Types.Type>) 
    (declaredFunctions : Map<Ast.Identifier, Types.Type>) 
        : TypeInferenceAst.Statement = 
    match statement with
    | Ast.ValueDeclaration(id, t, expr) 
        -> TypeInferenceAst.ValueDeclaration(id, t, createAnnotatedExpr expr (inferExpressionType expr declaredVariables))
    | Ast.FunctionCallStatement(id, args)
        ->  
        


let inferLocalTypes (func: Ast.FunctionDeclaration) : TypeInferenceAst.FunctionDeclaration = 
    let (id, pars, ret, cs) = func
    let inferredCompoundStatement = 
        cs 
        |> List.map processStatement
        {
            CompoundStatement = inferredCompoundStatement;
            InferredReturnType = Types.createBasicType "int";
            Parameters = [Types.createBasicType "int"]
        }


