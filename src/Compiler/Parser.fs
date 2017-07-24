module Compiler.Parser
open Compiler.Ast
open FParsec
open System
open System.Text.RegularExpressions

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
        ("fun", stringReturn "fun" ());
        ]
let builtInTypesParsersDict =
    dict [ 
            ("bool", stringReturn "bool" Bool);
            ("int", stringReturn "int" Bool);
            ("float", stringReturn "float" Bool);
            ("string", stringReturn "string" Bool);
            ("void", stringReturn "void" Bool);
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
let leftParen = skipChar '(' .>> spaces
let rightParen = skipChar ')' .>> spaces

let (expression : Parser<Expression, unit>), expressionRef = createParserForwardedToRef()
let pArgumentList = between leftParen rightParen (sepBy expression (pchar ','))
let pFunctionCallExpression =
 identifier .>>. pArgumentList 
 |>> fun (funName, args) -> FunctionCallExpression(funName, args)

let pStringLiteral = 
    between (pstring "'") 
        (pstring "'") 
        (manySatisfy ((<>) ''')) 
    |>> StringLiteral
let pLiteralExpression = pStringLiteral |>> LiteralExpression

expressionRef := choice 
    [pFunctionCallExpression;
     pLiteralExpression]

let pExpressionStatement
    = expression |>> ExpressionStatement
let pStatement : Parser<Statement,unit> = 
    choice[pExpressionStatement] .>> spaces .>> skipChar ';'
let typeSpec = choice(builtInTypesParsersDict.Values)

let (pFunctionDeclaration : Parser<Declaration, unit>) = 
    let parameter =
     (identifier .>>. opt (skipChar ':' >>. typeSpec))
      |>> fun (id,t) -> ScalarVariableDeclaration(t, id)
    let parametersList =
     sepBy parameter spaces 

    let returnType = opt (skipChar ':' >>. typeSpec)
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