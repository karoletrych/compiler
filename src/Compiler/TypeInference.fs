module TypeInference

open Compiler
open Compiler.Parser

module Types =
    type Parameter = 
        { Type : Type }

    and Method = 
        { Parameters : Parameter list
          Statements : Ast.Statement list }

    and Type = 
        { Name : string
          Guid : System.Guid
          TypeParameters : Type list
          ImplementedInterfaces : Type list
          BaseClass : Type option
          Methods : Method list
          Fields : Field list }

    and Field = Type * string

    let createBasicType name : Type = 
        { Name = name
          Guid = System.Guid.NewGuid()
          TypeParameters = []
          ImplementedInterfaces = []
          BaseClass = None
          Methods = []
          Fields = [] }

    let createGenericType name typeParams : Type = 
        { Name = name
          Guid = System.Guid.NewGuid()
          TypeParameters = typeParams
          ImplementedInterfaces = []
          BaseClass = None
          Methods = []
          Fields = [] }
    let typeOfLiteral = 
        function 
        | Ast.IntLiteral(_) -> createBasicType "int"
        | Ast.FloatLiteral(_) -> createBasicType "float"
        | Ast.StringLiteral(_) -> createBasicType "string"
        | Ast.BoolLiteral(_) -> createBasicType "bool"

module InferenceAst =
    type ExpressionWithInferredType = {
        Expression : Ast.Expression;
        Type : Types.Type option;
    }

    type FunctionDeclaration = {
        CompoundStatement : CompoundStatement;
        InferredReturnType : Types.Type;
        InferredParameters : Types.Type list;
    }

    and Statement = 
      | FunctionCallStatement of FunctionCallExpression
      | CompoundStatement of CompoundStatement
      | IfStatement of ExpressionWithInferredType * Statement * Statement option
      | WhileStatement of ExpressionWithInferredType * Statement
      | ReturnStatement of ExpressionWithInferredType option
      | VariableDeclarationStatement of VariableDeclaration
      | BreakStatement

    and VariableDeclaration = 
      | VariableDeclaration of Ast.Identifier * Ast.TypeSpec option * ExpressionWithInferredType option
      | ValueDeclaration of Ast.Identifier * Ast.TypeSpec option * ExpressionWithInferredType

    and CompoundStatement = Statement list

    and Expression = 
      | IdentifierExpression of Ast.IdentifierRef
      | FunctionCallExpression of FunctionCallExpression
      | ScalarAssignmentExpression of Ast.IdentifierRef * ExpressionWithInferredType

      | BinaryExpression of ExpressionWithInferredType * Ast.BinaryOperator * ExpressionWithInferredType
      | UnaryExpression of Ast.UnaryOperator * ExpressionWithInferredType
      | ArrayIdentifierExpression of Ast.IdentifierRef * ExpressionWithInferredType
      | ArrayAllocationExpression of Ast.TypeSpec * ExpressionWithInferredType
      | LiteralExpression of Ast.Literal

    and FunctionCallExpression = Ast.Identifier * Ast.Arguments

    let exprWithType expr typ = {
       Expression = expr;
       Type = typ
    }

let lookupPreviouslyDeclaredType identifier : Types.Type option =
    None 

let lookupDeclaredFunctions identifier : Types.Type option =
    None 
let inferBinaryExpressionType e1 op e2 = 
    Some (Types.createBasicType "nosuchtype")
let rec inferExpressionType (expression : Ast.Expression) : Types.Type option =
    match expression with
    | Ast.LiteralExpression(le) -> Some (Types.typeOfLiteral le)
    | Ast.IdentifierExpression(ie) -> lookupPreviouslyDeclaredType ie
    | Ast.FunctionCallExpression(fc) -> lookupDeclaredFunctions fc
    | Ast.BinaryExpression(e1, op, e2) -> inferBinaryExpressionType (inferExpressionType e1) op (inferExpressionType e2) 

let processStatement (statement : Ast.Statement) : InferenceAst.Statement= 
    match statement with
    | Ast.VariableDeclarationStatement(vd) ->
        let var = 
            match vd with
            | Ast.VariableDeclaration(id, typ, expr) -> InferenceAst.VariableDeclaration(id, typ, {InferenceAst.Expression = expr; InferenceAst.Type = typ} )
            | Ast.ValueDeclaration(id, typ, expr) -> InferenceAst.ValueDeclaration(id, typ, InferenceAst.exprWithType expr typ)
        var |> VariableDeclarationStatement
        


let inferLocalTypes (func: Ast.FunctionDeclaration) : InferenceAst.FunctionDeclaration = 
    let (id, pars, ret, cs) = func
    let inferredCompoundStatement = 
        cs 
        |> List.map processStatement
    {CompoundStatement = inferredCompoundStatement;
     InferredReturnType = Types.createBasicType "int";
      InferredParameters = [Types.createBasicType "int"]}

let program = parse "
                fun main 
                {
                    val name = 'Karol';
                    val age = 22;
                    val weight = 65.1;
                }"

program;;
