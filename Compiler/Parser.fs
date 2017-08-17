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

module Char =
    let leftBrace = skipChar '{' .>> spaces
    let rightBrace = skipChar '}' .>> spaces
    let semicolon = skipChar ';' .>> spaces
    let colon = skipChar ':' .>> spaces
    let leftParen = skipChar '(' .>> spaces
    let rightParen = skipChar ')' .>> spaces
    let leftAngleBracket = skipChar '<' .>> spaces
    let rightAngleBracket = skipChar '>' .>> spaces
    let comma = skipChar ',' .>> spaces
    let equals = skipChar '=' .>> spaces

module Keyword =
    let nonAlphanumericWs = nextCharSatisfiesNot (isAsciiLetter) >>. spaces
    let pFun = skipString "fun" .>> nonAlphanumericWs
    let pReturn = skipString "return" .>> nonAlphanumericWs
    let pVar = skipString "var" .>> nonAlphanumericWs
    let pVal = skipString "val" .>> nonAlphanumericWs
    let pIf = skipString "if" .>> nonAlphanumericWs
    let pElse = skipString "else" .>> nonAlphanumericWs
    let pClass = skipString "class" .>> nonAlphanumericWs
    let pConstructor = skipString "construct" .>> nonAlphanumericWs
    let pNew = skipString "new" .>> nonAlphanumericWs
    let keywords = [ 
        "fun";
        "return";
        "var";
        "val";
        "if";
        "else";
        "class";
        "construct";
        "new";
    ]

let pIdentifier, pIdentifierRef = createParserForwardedToRef<Identifier, _>() 

module Types =
    let pTypeSpec, pTypeSpecRef = createParserForwardedToRef()
    let pNonGenericTypeSpec = pIdentifier |>> NonGenericTypeSpec
    let pGenericType =
         pNonGenericTypeSpec 
             .>>. between Char.leftAngleBracket Char.rightAngleBracket (sepBy1 pTypeSpec Char.comma)
             |>> (GenericCustomTypeSpec)
    let pNonGenericType = pNonGenericTypeSpec |>> (NonGenericCustomTypeSpec)
    // TODO: fix naming
    let pCustomTypeSpec = (attempt pGenericType) <|> (pNonGenericType)
    let pCustomType = (attempt pGenericType |>> CustomType) <|> (pNonGenericType |>> CustomType)
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
    pTypeSpecRef := choice(attempt pCustomType :: (List.ofSeq builtInTypesParsersDict.Values)) .>> spaces

pIdentifierRef := identifier (IdentifierOptions()) .>> spaces
    >>= (fun id -> 
        if not (List.contains id Keyword.keywords) && not (Types.builtInTypesParsersDict.Keys.Contains(id))
        then preturn id 
        else fail ("Identifier cannot be a keyword: " + id))
        |>> Identifier

module Expression = 
    let opp = new OperatorPrecedenceParser<Expression,_,_>()
    let pExpression = opp.ExpressionParser
    let pArgumentList = sepBy pExpression Char.comma 
    let pFunctionCallExpression = 
        pIdentifier 
        .>>. (Char.leftParen >>. pArgumentList .>> Char.rightParen)

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

    let pParenthesizedExpression = between Char.leftParen Char.rightParen pExpression

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

    let pAssignment = (pIdentifier .>> Char.equals) .>>. pExpression 
    let pAssignmentExpression = 
        pAssignment 
        |>> (fun (id, expr) -> AssignmentExpression(id, expr))
    let pNewExpression = Keyword.pNew >>. Types.pCustomTypeSpec .>>. between Char.leftParen Char.rightParen pArgumentList |>> NewExpression

    opp.TermParser <- choice [
        pNewExpression;
        Literal.pLiteralExpression;
        attempt pAssignmentExpression;
        attempt pFunctionCallExpression |>> (FunctionCallExpression);
        pIdentifierExpression;
        pParenthesizedExpression;
    ]



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
        let variableDeclaration =
            pipe3 
                (Keyword.pVar >>. pIdentifier)
                (opt (Char.colon >>. Types.pTypeSpec))
                (opt (Char.equals >>. Expression.pExpression))
                (fun x y z -> x,y,z) // TODO: add utility functions for tupling arguments
        (
            variableDeclaration >>=
                (fun (varName, t, expr)
                    -> match (varName, t, expr) with
                       | (name, Some t, Some expr) -> preturn (FullDeclaration(varName, t, expr))
                       | (name, Some t, None) -> preturn (DeclarationWithType(varName, t))
                       | (name, None, Some expr) -> preturn (DeclarationWithInitialization(varName, expr))
                       | (name, None, None) -> fail "Implicitly typed variable must be initialized")
        )  
    let pLocalValueDeclarationStatement = 
        pipe3
            (Keyword.pVal >>. pIdentifier)
            (opt (Char.colon >>. Types.pTypeSpec))
            (Char.equals >>. Expression.pExpression)
            (fun a b c -> a,b,c)

    pStatementRef := 
        choice
            [
                pIfStatement;
                (pLocalValueDeclarationStatement .>> Char.semicolon) |>> ValueDeclaration;
                (pLocalVariableDeclarationStatement .>> Char.semicolon) |>> VariableDeclaration
                pReturnStatement .>> Char.semicolon;
                attempt pFunctionCallStatement .>> Char.semicolon;
                pAssignmentStatement .>> Char.semicolon;
            ]
    
module Function = 
    let parameter =
        //TODO: add immutable parameters parsing
        Char.leftParen >>. pIdentifier  .>>. (Char.colon >>. Types.pTypeSpec) .>> Char.rightParen 
    let parametersList =
        many parameter

    let pFunctionDeclaration = 
        let returnType = opt (Char.colon >>. Types.pTypeSpec)
        let body =
            between 
                Char.leftBrace
                Char.rightBrace
                (many Statement.pStatement)

        pipe4 
            (Keyword.pFun >>. pIdentifier)
            parametersList 
            returnType
            body 
            (fun id pars ret body -> id,pars,ret,body)

let emptyListIfNone args = 
                    match args with
                    | Some (args) -> preturn args
                    | None -> preturn [] 

module Class =
    let pClassName = Types.pNonGenericTypeSpec
    let pGenericParameters = 
        let pGenericParameter = pIdentifier |>> GenericTypeParameter
        between Char.leftAngleBracket Char.rightAngleBracket 
            (sepBy pGenericParameter Char.comma)
    let pInheritanceDeclaration = 
        opt (Char.colon >>. sepBy1 Types.pTypeSpec Char.comma) 
            >>= emptyListIfNone
    
    let pClassBody = 
        let pConstructor =
            let pBaseCall =
                opt (Char.colon >>. (between Char.leftParen Char.rightParen 
                        (sepBy1 Expression.pExpression Char.comma))) 
                    >>= emptyListIfNone
            pipe3 
                (Keyword.pConstructor >>. Function.parametersList)
                pBaseCall
                (many Statement.pStatement)
                (fun pars baseCall body -> { Parameters = pars; BaseClassConstructorCall = baseCall; Statements = body})
        pipe4
            (many Statement.pLocalValueDeclarationStatement)
            (many Statement.pLocalVariableDeclarationStatement)
            (opt pConstructor)
            (many Function.pFunctionDeclaration)
            (fun a b c d -> a,b,c,d)

    let pClass : Parser<ClassDeclaration, unit> =
        pipe4
            (Keyword.pClass >>. pClassName)
            (between Char.leftBrace Char.rightBrace pClassBody)
            (opt pGenericParameters >>= emptyListIfNone)
            (opt pInheritanceDeclaration >>= emptyListIfNone)
            (fun name body genericParameters inheritanceDeclaration ->
            let values, variables, constructor, functions = body
            {
                                Type = name;
                                GenericTypeParameters = genericParameters;
                                BaseTypes = inheritanceDeclaration;
                                FieldsDeclarations = variables;
                                ValueDeclarations = values;
                                Constructor = constructor;
                                FunctionDeclarations = functions;
            })

let pDeclaration = 
    choice[
        (Function.pFunctionDeclaration) |>> FunctionDeclaration;
        Class.pClass |>> ClassDeclaration
        ]
let pProgram = spaces >>. many pDeclaration

let parseProgram program =
    //TODO: consider using runParserOnString or sth else
    run pProgram program

let parse (sourceCode : string) : Program = 
    sourceCode 
    |> removeComments
    |> parseProgram 
    |> function
        | Success(result, _, _) -> result
        | Failure(message, error, state) -> failwith ((message, error, state).ToString())