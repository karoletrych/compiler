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

let toList = 
    function
    | Some (args) -> preturn args
    | None -> preturn [] 


let emptyListIfNone2 x = 
    Option.toList x |> preturn

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
    let leftSquareBracket = skipChar '[' .>> spaces
    let rightSquareBracket = skipChar ']' .>> spaces

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
    let pImplements = skipString "implements" .>> nonAlphanumericWs
    let pExtends = skipString "extends" .>> nonAlphanumericWs
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
    let pNonGenericTypeSpec = pIdentifier 
    let pGenericArguments =  between Char.leftAngleBracket Char.rightAngleBracket (sepBy1 pTypeSpec Char.comma)
    let pGenericType =
         pNonGenericTypeSpec 
             .>>. pGenericArguments
             |>> (CustomType)
    let pNonGenericType = pNonGenericTypeSpec |>> fun t -> (CustomType(t, []))

    let convertToFullyQualifiedType =
        let rec qualifiers acc types =
            match types with
            | [lastTypeSpec] -> preturn (acc, lastTypeSpec)
            | head :: tail -> 
                match head with
                | CustomType(identifier, [])
                    -> qualifiers (identifier::acc) tail 
                | CustomType _
                    -> fail "No generic type is allowed as namespace qualifier"
            | [] -> fail "Should not happen..."
        qualifiers [] 

    let pQualifiedType = 
                sepBy (attempt pGenericType <|> pNonGenericType) Char.doubleColon
                      >>= convertToFullyQualifiedType
    let pCustomType = pQualifiedType |>> CustomTypeSpec
    let builtInTypesParsersDict =
        dict [ 
            ("bool", stringReturn "bool" Bool);
            ("char", stringReturn "char" Char);
            ("int", stringReturn "int" Int);
            ("float", stringReturn "float" Float);
            ("double", stringReturn "double" Double);
            ("string", stringReturn "string" String);
            ("void", stringReturn "void" Void);
            ("obj", stringReturn "obj" Object);
        ]
    pTypeSpecImpl := choice(attempt pCustomType :: (List.ofSeq builtInTypesParsersDict.Values)) .>> spaces

let isAsciiIdStart    = fun c -> isAsciiLetter c || c = '_'
let isAsciiIdContinue = fun c -> isAsciiLetter c || isDigit c || c = '_'
let identifier =
  identifier (
    IdentifierOptions(
        isAsciiIdStart = isAsciiIdStart,
        isAsciiIdContinue = isAsciiIdContinue,
        normalization = Text.NormalizationForm.FormKC,
        normalizeBeforeValidation = true,
        allowAllNonAsciiCharsInPreCheck = true))

pIdentifierImpl := identifier .>> spaces
>>= (fun id -> 
    if not (List.contains id Keyword.keywords)
     && not (Types.builtInTypesParsersDict.Keys.Contains(id))
    then preturn id 
    else fail ("Identifier cannot be a keyword: " + id))

module Expression = 
    let opp = new OperatorPrecedenceParser<Expression,_,_>()
    let pExpression = opp.ExpressionParser
    let pArgumentList = sepBy pExpression Char.comma
    let pFunctionCall = 
        pipe3
            pIdentifier
            ((opt Types.pGenericArguments) >>= toList)
            (between Char.leftParen Char.rightParen pArgumentList)
            (fun name genericArgs args -> {Name = name; GenericArguments = genericArgs; Arguments = args})

    let pFunctionCallExpression = 
        pFunctionCall 
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
    opp.AddOperator(InfixOperator("+",  spaces, 6, Associativity.Left, fun x y -> BinaryExpression(x, Plus, y)))
    opp.AddOperator(InfixOperator("-",  spaces, 6, Associativity.Left, fun x y -> BinaryExpression(x, Minus, y)))
    opp.AddOperator(InfixOperator("*",  spaces, 7, Associativity.Left, fun x y -> BinaryExpression(x, Multiplication, y)))
    opp.AddOperator(InfixOperator("/",  spaces, 7, Associativity.Left, fun x y -> BinaryExpression(x, Division, y)))
    opp.AddOperator(InfixOperator("%",  spaces, 7, Associativity.Left, fun x y -> BinaryExpression(x, Remainder, y)))

    opp.AddOperator(PrefixOperator("!", spaces, 8, true, fun x -> UnaryExpression(LogicalNegate, x))) 
    opp.AddOperator(PrefixOperator("-", spaces, 8, true, fun x -> UnaryExpression(Negate, x))) 

    opp.AddOperator(InfixOperator(".", lookAhead (spaces .>> pFunctionCall), 9, Associativity.Left,
                                 fun x y -> MemberExpression (MemberFunctionCall(x,y))))

    let pNewExpression = Keyword.pNew >>. Types.pTypeSpec .>>. between Char.leftParen Char.rightParen pArgumentList |>> NewExpression

    let pStaticMemberExpression =
     Types.pCustomType .>>. ((Char.colonDot >>. pFunctionCall) )
     |>> StaticMemberExpression

    let pListInitializerExpression =
             between 
                 Char.leftSquareBracket
                 Char.rightSquareBracket 
                 (sepBy pExpression Char.semicolon) |>> ListInitializerExpression

    opp.TermParser <- choice [
        pListInitializerExpression;
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

    let pFunctionCallStatement = Expression.pFunctionCall |>>  FunctionCallStatement
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
                       | (name, Some t, Some expr) -> preturn (FullDeclaration(name, t, expr))
                       | (name, Some t, None) -> preturn (DeclarationWithType(name, t))
                       | (name, None, Some expr) -> preturn (DeclarationWithInitialization(name, expr))
                       | (_, None, None) -> fail "Implicitly typed variable must be initialized")
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
     ((Char.colonDot >>. Expression.pFunctionCall) )
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

        pipe5 
            (Keyword.pFun >>. pIdentifier)
            (opt pGenericParameters >>= toList)
            parametersList 
            returnType
            body 
            (fun a b c d e 
                -> {
                    Name = a;
                    GenericParameters = b;
                    Parameters = c;
                    ReturnType = d;
                    Body = e
                }
            )


module Class =
    let pClassName = Types.pNonGenericTypeSpec
    let pInheritanceDeclaration = 
        opt (Keyword.pExtends >>. Types.pQualifiedType ) .>>.
        (opt (Keyword.pImplements >>. sepBy1 Types.pQualifiedType Char.comma) >>= toList)
    
    let pClassBody = 
        let pConstructor =
            let pBaseCall =
                opt (Char.colon >>. (between Char.leftParen Char.rightParen 
                        (sepBy1 Expression.pExpression Char.comma))) 
                    >>= toList
            pipe3 
                (Keyword.pConstructor >>. Function.parametersList)
                pBaseCall
                (between Char.leftBrace Char.rightBrace (many Statement.pStatement))
                (fun pars baseCall body -> { Parameters = pars; BaseClassConstructorCall = baseCall; Statements = body})
        let readonlyFieldDeclaration = 
            tuple3
                (Keyword.pVal >>. pIdentifier)
                (Char.colon >>. Types.pTypeSpec)
                (opt (Char.equals >>. Expression.pExpression))
        tuple3
            (many readonlyFieldDeclaration)
            (opt pConstructor)
            (many Function.pFunctionDeclaration)

    let pClass : Parser<Class, unit> =
        pipe4
            (Keyword.pClass >>. pClassName)
            (opt Function.pGenericParameters >>= toList)
            pInheritanceDeclaration
            (between Char.leftBrace Char.rightBrace pClassBody)
            (fun name genericParameters inheritanceDeclaration body ->
            let properties, constructor, functions = body
            {
                Name = name;
                GenericTypeParameters = genericParameters;
                BaseClass = fst inheritanceDeclaration;
                Properties = properties;
                Constructor = constructor;
                FunctionDeclarations = functions;
                ImplementedInterfaces = snd inheritanceDeclaration;
            })

let pDeclaration = 
    choice[
        (Function.pFunctionDeclaration) |>> FunctionDeclaration;
        Class.pClass |>> ClassDeclaration
        ]
let pProgram = spaces >>. many pDeclaration 

let parseProgram =
    removeComments >> run pProgram

open Compiler.CompilerResult
let parse = 
    parseProgram 
    >>
    function
    | ParserResult.Success(result, _, _) -> succeed result 
    | ParserResult.Failure(message, error, state) -> failure (ParsingError ((message, error, state).ToString()))
