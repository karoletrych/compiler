module Compiler.AstProcessing
open Compiler.Ast

let rec expressionCata 
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
    expression : 'r = 
    let recurse = 
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
    match expression with
        | AssignmentExpression(e1, e2) -> assignment ((recurse e1), (recurse e2))
        | BinaryExpression(e1, op, e2) -> binary ((recurse e1), op, (recurse e2))
        | FunctionCallExpression(fc) -> 
            let args = fc.Arguments |> List.map recurse
            functionCall (fc.Name, args, fc.GenericArguments) 
        | InferredTypeExpression(e,t) -> (recurse e, t) |> inferredType 
        | IdentifierExpression(ie) -> identifier ie 
        | LiteralExpression(le) -> literal le
        | ListInitializerExpression list -> list |> List.map recurse |> listInitializer
        | MemberExpression (MemberFunctionCall(e1,e2)) -> memberExpression (recurse e1,recurse e2)
        | NewExpression (t, args) -> newExpression (t, args |> List.map recurse)
        | StaticMemberExpression (t, call) -> 
            let args = call.Arguments |> List.map recurse
            staticMember (t, (call.Name, args, call.GenericArguments))
        | UnaryExpression(op, e) -> unary (op, (recurse e))
let rec statementCata 
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
    statement = 
    let recurse = 
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
    match statement with
    | FunctionCallStatement(call) ->
        functionCall (call.Name, call.Arguments , call.GenericArguments)
    | StaticFunctionCallStatement(t, call) ->
        staticFunctionCall (t, (call.Name, call.Arguments, call.GenericArguments))
    | ValueDeclaration(id, t, e) -> 
        valueDeclaration (id, t, e)
    | VariableDeclaration(vd) -> 
        match vd with
        | DeclarationWithInitialization (id, e) -> declarationWithInitialization (id,e)
        | DeclarationWithType(id, t) -> declarationWithType(id, t)
        | FullDeclaration(id, t, e) -> fullVariableDeclaration(id,t,e)
    | CompositeStatement(cs) ->
        cs |> List.map recurse |> composite
    | ReturnStatement e -> returnStatement e
    | AssignmentStatement(e1, e2) -> assignment (e1, e2)
    | BreakStatement -> breakStatement
    | IfStatement(e, s, elseS) -> ifStatement

