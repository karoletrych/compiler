module Compiler.AstProcessing
open Compiler.Ast

/// takes functions processing expression nodes into some type
/// then takes expressions and applies one of the functions
/// depending on expression type
/// if expression consists of another one 
/// the function is called recursively on it
let rec expressionCata 
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
    expression : 'r = 
    let recurse = 
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
    let (AstExpression expression) = expression
    match expression with
        | AssignmentExpression(assignee, e2) -> 
            match assignee with
            | IdentifierAssignee i -> identifier i
            | MemberFieldAssignee (e, i) -> assignment ((recurse e, i), recurse e2)
        | BinaryExpression(e1, op, e2) -> binary ((recurse e1), op, (recurse e2))
        | LocalFunctionCallExpression(fc) -> 
            let args = fc.Arguments |> List.map recurse
            functionCall (fc.Name, args, fc.GenericArguments) 
        | IdentifierExpression(ie) -> identifier ie 
        | LiteralExpression(le) -> literal le
        | ListInitializerExpression list -> list |> List.map recurse |> listInitializer
        | InstanceMemberExpression (e1, m) -> 
            match m with
            | MemberFunctionCall (fc) ->  
                let args = fc.Arguments |> List.map recurse
                memberFunctionCall (recurse e1, (fc.Name, args, fc.GenericArguments))
            | MemberField (f) ->
                memberField (recurse e1, f)
        | NewExpression (t, args) -> newExpression (t, args |> List.map recurse)
        | StaticMemberExpression (t, m) -> 
             match m with
             | MemberFunctionCall (fc) ->  
                let args = fc.Arguments |> List.map recurse
                staticMemberFunctionCall (t, (fc.Name, args, fc.GenericArguments))
             | MemberField (f) ->
                staticMemberField (t, f)
        | UnaryExpression(op, e) -> unary (op, (recurse e))

/// same as above but for statements
let rec statementCata 
    functionCall
    staticFunctionCall
    valueDeclaration
    declarationWithInitialization
    declarationWithType
    fullVariableDeclaration
    composite
    returnStatement
    identifierAssignment
    fieldMemberAssignment
    ifStatement
    whileStatement
    instanceFunctionCall
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
            identifierAssignment
            fieldMemberAssignment  
            ifStatement
            whileStatement
            instanceFunctionCall
    match statement with
    | LocalFunctionCallStatement(call) ->
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
    | AssignmentStatement(assignee, e2) -> 
        match assignee with
            | IdentifierAssignee i -> identifierAssignment (i, e2)
            | MemberFieldAssignee (e, i) -> fieldMemberAssignment ((e, i), e2)
    | IfStatement (e,s,elseS) -> ifStatement (e, recurse s, elseS |> Option.map recurse)
    | InstanceMemberFunctionCallStatement(expr, fc) -> instanceFunctionCall (expr, fc)
    | WhileStatement(e, s) -> whileStatement (e, recurse s)

/// takes:
/// - functions which take accumulator and process statement
/// - accumulator
/// - statement
/// passes accumulator to every recursive call
let rec statementFold
    functionCall
    staticFunctionCall
    valueDeclaration
    declarationWithInitialization
    declarationWithType
    fullVariableDeclaration
    returnStatement
    assignment
    ifExpression
    whileStatement
    instanceFunctionCall
    ifStatement
    (acc : 'acc)
    statement = 
    let recurse = 
        statementFold
            functionCall
            staticFunctionCall
            valueDeclaration
            declarationWithInitialization
            declarationWithType
            fullVariableDeclaration
            returnStatement
            assignment  
            ifExpression
            whileStatement
            instanceFunctionCall
            ifStatement
    match statement with
    | LocalFunctionCallStatement(call) ->
        functionCall acc (call.Name, call.Arguments , call.GenericArguments)
    | StaticFunctionCallStatement(t, call) ->
        staticFunctionCall acc (t, (call.Name, call.Arguments, call.GenericArguments))
    | ValueDeclaration(id, t, e) -> 
        valueDeclaration acc (id, t, e)
    | VariableDeclaration(vd) -> 
        match vd with
        | DeclarationWithInitialization (id, e) -> declarationWithInitialization acc (id,e)
        | DeclarationWithType(id, t) -> declarationWithType acc (id, t)
        | FullDeclaration(id, t, e) -> fullVariableDeclaration acc (id,t,e)
    | CompositeStatement(cs) ->
        cs 
        |> List.fold recurse acc
    | ReturnStatement e -> returnStatement acc e
    | AssignmentStatement(e1, e2) -> assignment acc (e1, e2)
    | IfStatement (e,s,elseS) -> 
        let newAcc = ifExpression acc e
        ifStatement (recurse newAcc s, (elseS |> Option.map (recurse newAcc)))
    | InstanceMemberFunctionCallStatement(callee, mfc) -> instanceFunctionCall acc (callee, mfc)
    | WhileStatement(e, s) -> 
        let newAcc = whileStatement acc e
        recurse newAcc s
