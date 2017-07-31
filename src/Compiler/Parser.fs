module Compiler.Parser
open System
open System.Text.RegularExpressions
open Compiler.Ast
open FParsec

let removeComments input =  
    let blockComments = @"/\*(.*?)\*/";
    let lineComments = @"//(.*?)\r?\n";
    Regex.Replace(input, blockComments + "|" + lineComments, 
        (fun (me : Match) -> 
            if me.Value.StartsWith(@"//") 
            then Environment.NewLine 
            else ""),
            RegexOptions.Multiline)

let nonAlphanumeric = nextCharSatisfiesNot (isAsciiLetter)

module Keyword =
    let keywordParsers =
        dict [ 
            ("fun", pstring "fun");
            ("return", pstring "return")
            ("var", pstring "var")
            ("val", pstring "val")
            ]
    let pKeyword keywordName = keywordParsers.[keywordName] .>> nonAlphanumeric .>> spaces

let builtInTypesParsersDict =
    dict [ 
            ("bool", stringReturn "bool" Bool);
            ("char", stringReturn "char" Char);
            ("int", stringReturn "int" Int);
            ("float", stringReturn "float" Float);
            ("double", stringReturn "double" Double);
            ("string", stringReturn "string" String);
            ("void", stringReturn "void" Void);
         ]

let identifier = 
    identifier (IdentifierOptions()) .>> spaces
        >>= (fun (id:Identifier) -> 
            if not (Keyword.keywordParsers.ContainsKey(id))
            then preturn id
            else fail "Identifier cannot be a keyword")

let pIdentifier =
   (many1Satisfy isLower .>> nonAlphanumeric .>> spaces) // [a-z]+
   >>= (fun s -> if not (Keyword.keywordParsers.ContainsKey(s)) 
                   then (preturn s) 
                   else fail "a keyword cannot be an identifier")


let leftBrace = skipChar '{' .>> spaces
let rightBrace = skipChar '}' .>> spaces
let semicolon = skipChar ';' .>> spaces
let colon = skipChar ':' .>> spaces
let leftParen = skipChar '(' .>> spaces
let rightParen = skipChar ')' .>> spaces
let comma = skipChar ',' .>> spaces



//  EXPRESSIONS 
//  ----------
let expression, expressionRef = createParserForwardedToRef()
let pArgumentList = (sepBy expression comma)
let pFunctionCallExpression = 
    pipe2 
        identifier 
        (leftParen >>. pArgumentList .>> rightParen)
        (fun funName args -> FunctionCallExpression(funName, args))

let pStringLiteral = 
    (between (pstring "'") 
        (pstring "'") 
        (manySatisfy ((<>) ''')) )
        .>> spaces
    |>> StringLiteral
let pLiteralExpression = pStringLiteral |>> LiteralExpression

let pParenthesizedExpression = between leftParen rightParen expression

let pIdentifierExpression = 
    identifier 
    |>> fun id -> IdentifierExpression(
                    {
                        Identifier=id
                    })

expressionRef := choice 
    [
        attempt pFunctionCallExpression;
        pIdentifierExpression;
        pLiteralExpression;
        pParenthesizedExpression
    ]

let pTypeSpec = choice(builtInTypesParsersDict.Values) .>> spaces

module Statement =
        let pExpressionStatement = expression |>> ExpressionStatement

        let pReturnStatement =
            Keyword.pKeyword "return" >>. opt expression 
            |>> fun expr -> ReturnStatement(expr)
        let pLocalVariableDeclarationStatement = 
            Keyword.pKeyword "var" >>. identifier  .>>. opt (colon >>. pTypeSpec)
            |>> fun (varName,typ) -> ScalarVariableDeclaration(varName, typ, false) |> VariableDeclarationStatement

        let pLocalValueDeclarationStatement : Parser<Statement, unit> = 
            Keyword.pKeyword "val" >>. identifier  .>>. opt (colon >>. pTypeSpec)
            |>> fun (varName,typ) -> ScalarVariableDeclaration(varName, typ, true) |> VariableDeclarationStatement

        let pStatement = 
            choice
                [
                    pLocalValueDeclarationStatement;
                    pLocalVariableDeclarationStatement
                    pReturnStatement;
                    pExpressionStatement;
                ] .>> semicolon
    

let pFunctionDeclaration = 
    let parameter =
        let parenthesizedTypeParameter = leftParen >>. identifier  .>>. opt (colon >>. pTypeSpec) .>> rightParen 
                                        |>> fun (id, typ) -> ScalarVariableDeclaration(id, typ, true)
        let implicitTypeParameter = identifier |>> fun id -> ScalarVariableDeclaration(id, None, true)
        parenthesizedTypeParameter <|> implicitTypeParameter
    let parametersList =
        many parameter

    let returnType = opt (colon >>. pTypeSpec)
    let (body : Parser<Statement list, unit>)    
        = between 
            leftBrace
            rightBrace
            (many Statement.pStatement)

    pipe4 
        (Keyword.pKeyword "fun" >>. identifier)
        parametersList 
        returnType
        body 
        (fun id pars ret body
            -> FunctionDeclaration(id, pars, ret, body))
let pDeclaration = choice[pFunctionDeclaration]
let pProgram = spaces >>. many pDeclaration

let parseProgram program =
    //TODO consider using runParserOnString or sth else
    run pProgram program

let parse (sourceCode : string) : Program = 
    sourceCode 
    |> removeComments
    |> parseProgram 
    |> function
        | Success(result, _, _)   -> result
        | Failure(message, error, state) -> failwith ((message, error, state).ToString())