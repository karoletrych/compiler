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

let keywordParsers =
    dict [ 
        ("fun", pstring "fun");
        ("return", pstring "return")
        ]
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
            if not (keywordParsers.ContainsKey(id))
            then preturn id
            else fail "Identifier cannot be a keyword")
let nonAlphanumeric = nextCharSatisfiesNot (isAsciiLetter)
let keyword keywordName = keywordParsers.[keywordName] .>> nonAlphanumeric .>> spaces

let pIdentifier =
   (many1Satisfy isLower .>> nonAlphanumeric .>> spaces) // [a-z]+
   >>= (fun s -> if not (keywordParsers.ContainsKey(s)) 
                   then (preturn s) 
                   else fail "a keyword cannot be an identifier")


let leftBrace = skipChar '{' .>> spaces
let rightBrace = skipChar '}' .>> spaces
let semicolon = skipChar ';' .>> spaces
let colon = skipChar ':' .>> spaces
let leftParen = skipChar '(' .>> spaces
let rightParen = skipChar ')' .>> spaces
let comma = skipChar ',' .>> spaces



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

let pExpressionStatement =
    expression |>> ExpressionStatement

let pReturnStatement =
    keyword "return" >>. opt expression 
    |>> fun expr -> ReturnStatement(expr)
let pStatement = 
    choice[pReturnStatement; pExpressionStatement] .>> semicolon
let typeSpec = choice(builtInTypesParsersDict.Values) .>> spaces

let pFunctionDeclaration = 
    let parameter =
        let parenthesizedTypeParameter = leftParen >>. identifier  .>>. opt (colon >>. typeSpec) .>> rightParen |>> fun (id, typ) -> ScalarVariableDeclaration(typ, id)
        let implicitTypeParameter = identifier |>> fun id -> ScalarVariableDeclaration(None, id)
        parenthesizedTypeParameter <|> implicitTypeParameter
    let parametersList =
        many parameter

    let returnType = opt (colon >>. typeSpec)
    let (body : Parser<Statement list, unit>)    
        = between 
            leftBrace
            rightBrace
            (many pStatement)

    pipe4 
        (keyword "fun" >>. identifier)
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