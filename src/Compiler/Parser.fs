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


module Keyword =
    let nonAlphanumericWs = nextCharSatisfiesNot (isAsciiLetter) >>. spaces
    let pFun = skipString "fun" .>> nonAlphanumericWs
    let pReturn = skipString "return" .>> nonAlphanumericWs
    let pVar = skipString "var" .>> nonAlphanumericWs
    let pVal = skipString "val" .>> nonAlphanumericWs
    let pIf = skipString "if" .>> nonAlphanumericWs
    let pElse = skipString "else" .>> nonAlphanumericWs
    let keywordParsers = [ 
        "fun";
        "return";
        "var";
        "val";
        "if";
        "else";
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

let pIdentifier = 
    identifier (IdentifierOptions()) .>> spaces
        >>= (fun (id:Identifier) -> 
            if not (Keyword.keywordParsers |> List.contains id)
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
    let pFunctionCallExpression = 
        pIdentifier 
        .>>. (leftParen >>. pArgumentList .>> rightParen)

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
        |>> fun id -> IdentifierExpression(id)

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

    let pAssignment = (pIdentifier .>> equals) .>>. pExpression 
    let pAssignmentExpression = 
        pAssignment 
        |>> (fun (id, expr) -> AssignmentExpression(id, expr))

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

    let pElseStatement = Keyword.pElse >>. pStatement
    let pIfStatement = pipe3 
                        (Keyword.pIf >>. Expression.pExpression) 
                        (pStatement) 
                        (opt pElseStatement)
                        (fun expression statement elseSt -> IfStatement(expression, statement, elseSt))

    let pFunctionCallStatement = Expression.pFunctionCallExpression |>> FunctionCallStatement
    let pAssignmentStatement = Expression.pAssignment |>> AssignmentStatement

    let pReturnStatement =
        Keyword.pReturn >>. opt Expression.pExpression 
        |>> fun expr -> ReturnStatement(expr)
    let pLocalVariableDeclarationStatement = 
        pipe3 
            (Keyword.pVar >>. pIdentifier)
            (opt (colon >>. pTypeSpec))
            (opt (equals >>. Expression.pExpression))
            (fun varName typ expr -> VariableDeclaration(varName, typ, expr))

    let pLocalValueDeclarationStatement = 
        pipe3
            (Keyword.pVal >>. pIdentifier)
            (opt (colon >>. pTypeSpec))
            (equals >>. Expression.pExpression)
            (fun valName typ expr -> ValueDeclaration(valName, typ, expr))

    pStatementRef := 
        choice
            [
                pIfStatement;
                pLocalValueDeclarationStatement .>> semicolon;
                pLocalVariableDeclarationStatement .>> semicolon
                pReturnStatement .>> semicolon;
                attempt pFunctionCallStatement .>> semicolon;
                pAssignmentStatement .>> semicolon;
            ]
    

let pFunctionDeclaration = 
    let parameter =
        //TODO: add immutable parameters parsing
        leftParen >>. pIdentifier  .>>. (colon >>. pTypeSpec) .>> rightParen 
                                        |>> fun (id, typ) -> (id, typ)
    let parametersList =
        many parameter

    let returnType = opt (colon >>. pTypeSpec)
    let (body : Parser<Statement list, unit>)    
        = between 
            leftBrace
            rightBrace
            (many Statement.pStatement)

    pipe4 
        (Keyword.pFun >>. pIdentifier)
        parametersList 
        returnType
        body 
        (fun id pars ret body
            -> FunctionDeclaration(id, pars, ret, body))
let pDeclaration = choice[pFunctionDeclaration]
let pProgram = spaces >>. many pDeclaration

let parseProgram program =
    //TODO: consider using runParserOnString or sth else
    run pProgram program

let parse (sourceCode : string) : Program = 
    sourceCode 
    |> removeComments
    |> parseProgram 
    |> function
        | Success(result, _, _)   -> result
        | Failure(message, error, state) -> failwith ((message, error, state).ToString())