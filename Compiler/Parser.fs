#if !(INTERACTIVE)
module Compiler.Parser
#endif

#if INTERACTIVE
#r @"../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r @"../packages/FParsec/lib/net40-client/FParsec.dll"
#load @"Ast.fs"
#endif

open System
open System.Text.RegularExpressions
open Compiler.Ast
open FParsec


//TODO: unify places of constructing union cases (in parser declaration vs in usage)

let removeComments input =  
    let blockComments = @"/\*(.*?)\*/";
    let lineComments = @"//(.*?)\r?\n";
    Regex.Replace(input, blockComments + "|" + lineComments, 
        (fun (me : Match) -> 
            if me.Value.StartsWith(@"//") 
            then Environment.NewLine 
            else ""),
            RegexOptions.Multiline)

let emptyListIfNone args = 
                    match args with
                    | Some (args) -> preturn args
                    | None -> preturn [] 

module Char =
    let leftBrace = skipChar '{' .>> spaces
    let rightBrace = skipChar '}' .>> spaces
    let semicolon = skipChar ';' .>> spaces
    let colon = skipChar ':' .>> spaces
    let doubleColon = skipString "::" .>> spaces
    let colonDot = skipString ":." .>> spaces
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
        "true";
        "false"
    ]

let pIdentifier, pIdentifierImpl = createParserForwardedToRef<Identifier, _>() 

module Types =
    let pTypeSpec, pTypeSpecImpl = createParserForwardedToRef()
    let pNonGenericTypeSpec = pIdentifier |>> NonGenericTypeSpec
    let pGenericArguments =  between Char.leftAngleBracket Char.rightAngleBracket (sepBy1 pTypeSpec Char.comma)
    let pGenericType =
         pNonGenericTypeSpec 
             .>>. pGenericArguments
             |>> (GenericCustomTypeSpec)
    let pNonGenericType = pNonGenericTypeSpec |>> (NonGenericCustomTypeSpec)

    let convertToFullyQualifiedType types =
        let rec qualifiers types acc : Parser<Identifier list * CustomTypeSpec, unit> =
            match types with
            | [lastTypeSpec] -> preturn (acc, lastTypeSpec)
            | head :: tail -> 
                match head with
                | GenericCustomTypeSpec (gcts, t)
                    -> fail "No generic type is allowed as namespace qualifier"
                | NonGenericCustomTypeSpec(NonGenericTypeSpec(identifier))
                    -> qualifiers tail (identifier::acc)
            | [] -> fail "Should not happen..."
        qualifiers types []

    let pCustomType = 
                sepBy (attempt pGenericType <|> pNonGenericType) Char.doubleColon
                      >>= convertToFullyQualifiedType
                      |>> CustomType
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
    pTypeSpecImpl := choice(attempt pCustomType :: (List.ofSeq builtInTypesParsersDict.Values)) .>> spaces

let isAsciiIdStart    = fun c -> isAsciiLetter c || c = '_'
let isAsciiIdContinue = fun c -> isAsciiLetter c || isDigit c || c = '_'
let identifier =
  identifier (
    IdentifierOptions(
        isAsciiIdStart = isAsciiIdStart,
        isAsciiIdContinue = isAsciiIdContinue,
        normalization = System.Text.NormalizationForm.FormKC,
        normalizeBeforeValidation = true,
        allowAllNonAsciiCharsInPreCheck = true))

pIdentifierImpl := identifier .>> spaces
>>= (fun id -> 
    if not (List.contains id Keyword.keywords) && not (Types.builtInTypesParsersDict.Keys.Contains(id))
    then preturn id 
    else fail ("Identifier cannot be a keyword: " + id))
    |>> Identifier

module Expression = 
    let opp = new OperatorPrecedenceParser<Expression,_,_>()
    let pExpression = opp.ExpressionParser
    let pArgumentList = sepBy pExpression Char.comma
    let pFunctionCall = 
        tuple3
            pIdentifier
            ((opt Types.pGenericArguments) >>= emptyListIfNone)
            (between Char.leftParen Char.rightParen pArgumentList)

    let pFunctionCallExpression = 
        pFunctionCall 
        |>> FunctionCall 
        |>> FunctionCallExpression
    module Literal =
        let pStringLiteral = 
            (between (pstring "\"") 
                (pstring "\"") 
                (manySatisfy ((<>) '\"')) )
                .>> spaces
            |>> StringLiteral

        let pFloatLiteral = pfloat |>> FloatLiteral
        let pIntLiteral = (pint32 .>> notFollowedBy (pstring ".")) .>> spaces |>> IntLiteral
        let pBoolParser = (pstring "true" >>% BoolLiteral true) <|> (pstring "false" >>% BoolLiteral false)
        let pLiteralExpression = choice [pBoolParser; attempt pIntLiteral; pFloatLiteral; pStringLiteral] |>> LiteralExpression

    let pParenthesizedExpression = between Char.leftParen Char.rightParen pExpression

    let pIdentifierExpression = 
        pIdentifier 
            |>> fun id -> IdentifierExpression(id)

    opp.AddOperator(InfixOperator("=", spaces, 1, Associativity.Right, fun x y -> AssignmentExpression(x, y))) 

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

    opp.AddOperator(InfixOperator(".", lookAhead (spaces .>> pFunctionCall), 9, Associativity.Left,
                                 fun x y -> MemberExpression (MemberFunctionCall(x,y))))

    let pNewExpression = Keyword.pNew >>. Types.pTypeSpec .>>. between Char.leftParen Char.rightParen pArgumentList |>> NewExpression

    let pStaticMemberExpression =
     Types.pCustomType .>>. ((Char.colonDot >>. pFunctionCall) |>> FunctionCall)
     |>> StaticMemberExpression

    opp.TermParser <- choice [
        attempt pStaticMemberExpression;
        pNewExpression;
        Literal.pLiteralExpression;
        attempt pFunctionCallExpression;
        pIdentifierExpression;
        pParenthesizedExpression
    ] 

module Statement =
    let pStatement, pStatementRef = createParserForwardedToRef()

    let pElseStatement = Keyword.pElse >>. pStatement
    let pIfStatement = pipe3 
                        (Keyword.pIf >>. Expression.pExpression) 
                        (pStatement) 
                        (opt pElseStatement)
                        (fun expression statement elseSt -> IfStatement(expression, statement, elseSt))

    let pFunctionCallStatement = Expression.pFunctionCall |>> FunctionCall |>> FunctionCallStatement
    let pReturnStatement =
        Keyword.pReturn >>. opt Expression.pExpression 
        |>> fun expr -> ReturnStatement(expr)
    let pLocalVariableDeclarationStatement = 
        let variableDeclaration =
            tuple3 
                (Keyword.pVar >>. pIdentifier)
                (opt (Char.colon >>. Types.pTypeSpec))
                (opt (Char.equals >>. Expression.pExpression))
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
        tuple3
            (Keyword.pVal >>. pIdentifier)
            (opt (Char.colon >>. Types.pTypeSpec))
            (Char.equals >>. Expression.pExpression)

    let expressionStatement =
     Expression.pExpression
     >>= (fun expr ->
                 match expr with
                 |MemberExpression(me) -> preturn (MemberFunctionCallStatement me)
                 |AssignmentExpression(e1,e2) -> preturn (AssignmentStatement (e1, e2))
                 |FunctionCallExpression(fc) -> preturn (FunctionCallStatement (fc))
                 | _ -> fail "given expression cannot be a statement" )

    let pStaticFunctionCallStatement =
     attempt Types.pCustomType .>>.
     ((Char.colonDot >>. Expression.pFunctionCall) |>> FunctionCall)
     |>> StaticFunctionCallStatement

    pStatementRef := 
        choice
            [
                pIfStatement;
                (pLocalValueDeclarationStatement .>> Char.semicolon) |>> ValueDeclaration;
                (pLocalVariableDeclarationStatement .>> Char.semicolon) |>> VariableDeclaration
                pReturnStatement .>> Char.semicolon;
                attempt pStaticFunctionCallStatement .>> Char.semicolon;
                attempt expressionStatement .>> Char.semicolon;
                attempt pFunctionCallStatement .>> Char.semicolon;
            ]
    
module Function = 
    let parameter =
        //TODO: add immutable parameters parsing
        Char.leftParen >>. pIdentifier  .>>. (Char.colon >>. Types.pTypeSpec) .>> Char.rightParen 
    let parametersList =
        many parameter

    let pGenericParameters = 
        let pGenericParameter = pIdentifier |>> GenericTypeParameter
        between Char.leftAngleBracket Char.rightAngleBracket 
            (sepBy pGenericParameter Char.comma)
    let pFunctionDeclaration = 
        let returnType = opt (Char.colon >>. Types.pTypeSpec)
        let body =
            between 
                Char.leftBrace
                Char.rightBrace
                (many Statement.pStatement)

        tuple5 
            (Keyword.pFun >>. pIdentifier)
            (opt pGenericParameters >>= emptyListIfNone)
            parametersList 
            returnType
            body 


module Class =
    let pClassName = Types.pNonGenericTypeSpec
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
                (between Char.leftBrace Char.rightBrace (many Statement.pStatement))
                (fun pars baseCall body -> { Parameters = pars; BaseClassConstructorCall = baseCall; Statements = body})
        let readonlyFieldDeclaration = 
            tuple3
                (Keyword.pVal >>. pIdentifier)
                (opt (Char.colon >>. Types.pTypeSpec))
                (opt (Char.equals >>. Expression.pExpression))
        tuple4
            (many readonlyFieldDeclaration)
            (many Statement.pLocalVariableDeclarationStatement)
            (opt pConstructor)
            (many Function.pFunctionDeclaration)

    let pClass : Parser<ClassDeclaration, unit> =
        pipe4
            (Keyword.pClass >>. pClassName)
            (opt Function.pGenericParameters >>= emptyListIfNone)
            (opt pInheritanceDeclaration >>= emptyListIfNone)
            (between Char.leftBrace Char.rightBrace pClassBody)
            (fun name genericParameters inheritanceDeclaration body ->
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
