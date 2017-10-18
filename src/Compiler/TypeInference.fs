module Compiler.TypeInference

// open Compiler
// open Compiler.Parser
// open Compiler.TypeInferenceAst

// let lookupPreviouslyDeclaredType identifier declaredVariables : Types.Type option =
//     None 

// let lookupDeclaredFunctions identifier : Types.Type option =
//     failwith "notimplemented" 
// let inferBinaryExpressionType e1 op e2 = 
//     failwith "not implemented"
// let inferUnaryExpressionType op e = 
//     failwith "not implemented"

// let rec inferExpressionType 
//     (expression : Ast.Expression) 
//     (declaredVariables : Map<Ast.Identifier, Types.Type>) 
//         : Types.Type option =
//     let lookupType = lookupPreviouslyDeclaredType declaredVariables

//     match expression with
//         | Ast.IdentifierExpression(ie) -> lookupType ie 
//         | Ast.FunctionCallExpression(fc) -> lookupDeclaredFunctions fc
//         | Ast.LiteralExpression(le) -> Some (Types.typeOfLiteral le)
//         | Ast.BinaryExpression(e1, op, e2) -> inferBinaryExpressionType (inferExpressionType e1) op (inferExpressionType e2)
//         | Ast.UnaryExpression(op, e) -> inferUnaryExpressionType op (inferExpressionType e)
//         | Ast.AssignmentExpression(id, e) -> inferExpressionType e declaredVariables

// let resolveType t = 
//     Some (Types.createBasicType (t.ToString()))

// let createAnnotatedExpr e typ = {Expression = e; Type = typ}
// let processStatement 
//     (statement : Ast.Statement)
//     (declaredVariables : Map<Ast.Identifier, Types.Type>) 
//     (declaredFunctions : Map<Ast.Identifier, Types.Type>) 
//         : TypeInferenceAst.Statement = 
//     match statement with
//     | Ast.ValueDeclaration(id, t, expr) 
//         -> TypeInferenceAst.ValueDeclaration(id, t, createAnnotatedExpr expr (inferExpressionType expr declaredVariables))
//     | Ast.FunctionCallStatement(id, args)
//         ->  
        


// let inferLocalTypes (func: Ast.FunctionDeclaration) : TypeInferenceAst.FunctionDeclaration = 
//     let (id, pars, ret, cs) = func
//     let inferredCompoundStatement = 
//         cs 
//         |> List.map processStatement
//         {
//             CompoundStatement = inferredCompoundStatement;
//             InferredReturnType = Types.createBasicType "int";
//             Parameters = [Types.createBasicType "int"]
//         }

// let program = parse @"
//                 fun main 
//                 {
//                     val name = 'Karol';
//                     val age = 22;
//                     val weight = 65.1;
//                     val arr = [1;2;3;""string""]
//                 }"

// program;;

type T = {
    Base : T list;
    Name : string;
}
let o = { Base = []; Name = "Object"}
let str = {Base = [o]; Name = "String"}
let int = {Base = [o]; Name = "int"}
let exc = { Base = [o]; Name = "exception"}
let runtimeException = {
    Base = [exc]; Name = "RuntimeException"
    }
let anotherException = {
    Base = [exc]; Name = "AnotherException"
    }
let iface = {
    Base = [o]; Name = "Interface"
    }
let classA = {
    Base = [o;iface]; Name = "A"
    }

let classB = {
    Base = [o;iface]; Name = "B"
    }

let allTypes = [
 o;
 exc;
 runtimeException;
 str;
 int;
 anotherException
 ]
let rec allAncestors t : T list  =
    let {Base = baseTypes; Name = name} = t
    match baseTypes with 
    | [] -> [] 
    | b -> b @ (b |> List.collect allAncestors)

let children allNodes parentNode = 
            allNodes 
            |> List.filter (fun t -> 
                let {Base = baseTypes; Name = name} = t
                baseTypes |> List.contains parentNode
                )

let rec longestPath root allNodes =
    let childrenOf = children allNodes
    let isALeaf = childrenOf root = List.empty
    if isALeaf then
        ([root], 0)
    else
        let edges = childrenOf root |> List.map (fun child ->
            let (p, l) = longestPath child allNodes
            (child :: p, l + 1)
            )
        edges |> List.maxBy snd

let longest = longestPath o

let types = [classA; classB]

types
    |> List.map (fun t -> (t::allAncestors t))
    |> List.map Set.ofList
    |> Set.intersectMany
    |> Set.toList
    |> List.distinct
    |> longest
    |> fst
    |> List.last
    ;;
