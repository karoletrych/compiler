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
            ("if", pstring "if")
            ("else", pstring "else")
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

let pIdentifier = 
    identifier (IdentifierOptions()) .>> spaces
        >>= (fun (id:Identifier) -> 
            if not (Keyword.keywordParsers.ContainsKey(id))
            then preturn id
            else fail "Identifier cannot be a keyword")


let leftBrace = skipChar '{' .>> spaces
let rightBrace = skipChar '}' .>> spaces
let semicolon = skipChar ';' .>> spaces
let colon = skipChar ':' .>> spaces
let leftParen = skipChar '(' .>> spaces
let rightParen = skipChar ')' .>> spaces
let comma = skipChar ',' .>> spaces
let equals = skipChar '=' .>> spaces



module Expression = 

    let opp = new OperatorPrecedenceParser<Expression,_,_>()
    let pExpression = opp.ExpressionParser

    let pArgumentList = (sepBy pExpression comma)
    let pFunctionCallExpression : Parser<FunctionCallExpression, unit>= 
        pipe2 
            pIdentifier 
            (leftParen >>. pArgumentList .>> rightParen)
            (fun funName args -> (funName, args))

    module Literal =
        let pStringLiteral = 
            (between (pstring "'") 
                (pstring "'") 
                (manySatisfy ((<>) ''')) )
                .>> spaces
            |>> StringLiteral

        let pFloatLiteral = pfloat |>> FloatLiteral
        let pIntLiteral = (pint32 .>> notFollowedBy (pstring ".")) .>> spaces |>> IntLiteral
        let pLiteralExpression = choice [attempt pIntLiteral; pFloatLiteral; pStringLiteral] |>> LiteralExpression

    let pParenthesizedExpression = between leftParen rightParen pExpression

    let pIdentifierExpression = 
        pIdentifier 
        |>> fun id -> IdentifierExpression(
                        {
                            Identifier=id
                        })

    opp.AddOperator(InfixOperator("||", spaces, 2, Associativity.Left, fun x y -> BinaryExpression(x, ConditionalOr, y)))
    opp.AddOperator(InfixOperator("==", spaces, 3, Associativity.Left, fun x y -> BinaryExpression(x, Equal, y)))
    opp.AddOperator(InfixOperator("!=", spaces, 3, Associativity.Left, fun x y -> BinaryExpression(x, NotEqual, y)))
    opp.AddOperator(InfixOperator("<=", spaces, 4, Associativity.None, fun x y -> BinaryExpression(x, LessEqual, y)))
    opp.AddOperator(InfixOperator(">=", spaces, 4, Associativity.None, fun x y -> BinaryExpression(x, GreaterEqual, y)))
    opp.AddOperator(InfixOperator(">",  spaces, 4, Associativity.None, fun x y -> BinaryExpression(x, Greater, y)))
    opp.AddOperator(InfixOperator("<",  spaces, 4, Associativity.None, fun x y -> BinaryExpression(x, Less, y)))
    opp.AddOperator(InfixOperator("&&", spaces, 5, Associativity.Left, fun x y -> BinaryExpression(x, ConditionalAnd, y)))
    opp.AddOperator(InfixOperator("+",  spaces, 6, Associativity.Left, fun x y -> BinaryExpression(x, Add, y)))
    opp.AddOperator(InfixOperator("-",  spaces, 6, Associativity.Left, fun x y -> BinaryExpression(x, Subtract, y)))
    opp.AddOperator(InfixOperator("*",  spaces, 7, Associativity.Left, fun x y -> BinaryExpression(x, Multiply, y)))
    opp.AddOperator(InfixOperator("/",  spaces, 7, Associativity.Left, fun x y -> BinaryExpression(x, Divide, y)))
    opp.AddOperator(InfixOperator("%",  spaces, 7, Associativity.Left, fun x y -> BinaryExpression(x, Modulus, y)))

    opp.AddOperator(PrefixOperator("!", spaces, 8, true, fun x -> UnaryExpression(LogicalNegate, x))) 
    opp.AddOperator(PrefixOperator("-", spaces, 8, true, fun x -> UnaryExpression(Negate, x))) 
    opp.AddOperator(PrefixOperator("+", spaces, 8, true, fun x -> UnaryExpression(Identity, x))) 

    let pAssignmentExpression = pipe2
                                 (pIdentifier .>> equals)  
                                 pExpression 
                                 (fun id expr -> ScalarAssignmentExpression({Identifier = id}, expr))

    opp.TermParser <- choice [
        Literal.pLiteralExpression;
        attempt pAssignmentExpression;
        attempt pFunctionCallExpression |>> FunctionCallExpression;
        pIdentifierExpression;
        pParenthesizedExpression
    ]


let pTypeSpec = choice(builtInTypesParsersDict.Values) .>> spaces

module Statement =
    let pStatement, pStatementRef = createParserForwardedToRef()

    let pElseStatement = Keyword.pKeyword "else" >>. pStatement
    let pIfStatement = pipe3 
                        (Keyword.pKeyword "if" >>. Expression.pExpression) 
                        (pStatement) 
                        (opt pElseStatement)
                        (fun expression statement elseSt -> IfStatement(expression, statement, elseSt))

    let pExpressionStatement = Expression.pFunctionCallExpression |>> FunctionCallStatement

    let pReturnStatement =
        Keyword.pKeyword "return" >>. opt Expression.pExpression 
        |>> fun expr -> ReturnStatement(expr)
    let pLocalVariableDeclarationStatement = 
        pipe3 
            (Keyword.pKeyword "var" >>. pIdentifier)
            (opt (colon >>. pTypeSpec))
            (opt (equals >>. Expression.pExpression))
            (fun varName typ expr -> VariableDeclaration(varName, typ, expr) |> VariableDeclarationStatement)

    let pLocalValueDeclarationStatement = 
        pipe3
            (Keyword.pKeyword "val" >>. pIdentifier)
            (opt (colon >>. pTypeSpec))
            (equals >>. Expression.pExpression)
            (fun valName typ expr -> ValueDeclaration(valName, typ, expr) |> VariableDeclarationStatement)

    pStatementRef := 
        choice
            [
                pIfStatement;
                pLocalValueDeclarationStatement .>> semicolon;
                pLocalVariableDeclarationStatement .>> semicolon
                pReturnStatement .>> semicolon;
                pExpressionStatement .>> semicolon;
            ]
    

let pFunctionDeclaration = 
    let parameter =
        let parenthesizedTypeParameter = leftParen >>. pIdentifier  .>>. opt (colon >>. pTypeSpec) .>> rightParen 
                                        |>> fun (id, typ) -> (id, typ)
        let implicitTypeParameter = pIdentifier |>> fun id -> (id, None)
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
        (Keyword.pKeyword "fun" >>. pIdentifier)
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