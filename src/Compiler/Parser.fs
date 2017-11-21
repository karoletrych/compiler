module Compiler.Parser

open System
open System.Text.RegularExpressions
open Ast
open FParsec

type SourceFile = {
    Name : string
    Code : string
}

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

let pIdentifier, pIdentifierImpl = createParserForwardedToRef<string, _>() 

module Types =
    let pTypeSpec, pTypeSpecImpl = createParserForwardedToRef()
    let pNonGenericTypeSpec = pIdentifier 
    let pGenericArguments =  between Char.leftAngleBracket Char.rightAngleBracket (sepBy1 pTypeSpec Char.comma)
    let pGenericType =
         pNonGenericTypeSpec 
             .>>. pGenericArguments
             |>> (fun (name, types) -> {Name = name; GenericArgs = types})
    let pNonGenericType = pNonGenericTypeSpec |>> fun t -> ({Name = t; GenericArgs = []})

    let convertToFullyQualifiedType =
        let rec qualifiers acc types =
            match types with
            | [lastTypeSpec] -> preturn (acc, lastTypeSpec)
            | head :: tail -> 
                match head with
                | {Name = identifier; GenericArgs = []}
                    -> qualifiers (identifier::acc) tail 
                | _
                    -> fail "No generic type is allowed as namespace qualifier"
            | [] -> fail "Should not happen..."
        qualifiers [] 

    let pQualifiedType = 
                sepBy (attempt pGenericType <|> pNonGenericType) Char.doubleColon
                      >>= convertToFullyQualifiedType
    let pCustomType = pQualifiedType |>> CustomTypeSpec
    let builtInTypesParsers =
        [ 
            ("bool", stringReturn "bool" Bool);
            ("char", stringReturn "char" Char);
            ("int", stringReturn "int" Int);
            ("float", stringReturn "float" Float);
            ("double", stringReturn "double" Double);
            ("string", stringReturn "string" String);
            ("void", stringReturn "void" Void);
            ("obj", stringReturn "obj" Object);
        ]
        |> Map.ofList
    pTypeSpecImpl := choice(attempt pCustomType :: (builtInTypesParsers |> Map.toList |> List.map (snd >> fun p -> p |>> fun bits -> bits |> BuiltInTypeSpec) )) .>> spaces

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
     && not (Types.builtInTypesParsers |> Map.toSeq |> Seq.map fst |> Seq.contains (id))
    then preturn id 
    else fail ("Identifier cannot be a keyword: " + id))

module Expression = 
    let opp = new OperatorPrecedenceParser<AstExpression,_,_>()
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
        

    let pParenthesizedExpression = 
        between Char.leftParen Char.rightParen pExpression
    let pIdentifierExpression = 
        pIdentifier 
            |>> fun id -> IdentifierExpression(id)

    opp.AddOperator(InfixOperator("=", spaces, 1, Associativity.Right, fun x y -> AssignmentExpression(x, y) |> AstExpression)) 

    opp.AddOperator(InfixOperator("||", spaces, 2, Associativity.Left, fun x y -> BinaryExpression(x, ConditionalOr, y) |> AstExpression))
    opp.AddOperator(InfixOperator("==", spaces, 3, Associativity.Left, fun x y -> BinaryExpression(x, Equal, y) |> AstExpression))
    opp.AddOperator(InfixOperator("!=", spaces, 3, Associativity.Left, fun x y -> BinaryExpression(x, NotEqual, y) |> AstExpression))
    opp.AddOperator(InfixOperator("<=", spaces, 4, Associativity.None, fun x y -> BinaryExpression(x, LessEqual, y) |> AstExpression))
    opp.AddOperator(InfixOperator(">=", spaces, 4, Associativity.None, fun x y -> BinaryExpression(x, GreaterEqual, y) |> AstExpression))
    opp.AddOperator(InfixOperator(">",  spaces, 4, Associativity.None, fun x y -> BinaryExpression(x, Greater, y) |> AstExpression))
    opp.AddOperator(InfixOperator("<",  spaces, 4, Associativity.None, fun x y -> BinaryExpression(x, Less, y) |> AstExpression))
    opp.AddOperator(InfixOperator("&&", spaces, 5, Associativity.Left, fun x y -> BinaryExpression(x, ConditionalAnd, y) |> AstExpression))
    opp.AddOperator(InfixOperator("+",  spaces, 6, Associativity.Left, fun x y -> BinaryExpression(x, Plus, y) |> AstExpression))
    opp.AddOperator(InfixOperator("-",  spaces, 6, Associativity.Left, fun x y -> BinaryExpression(x, Minus, y) |> AstExpression))
    opp.AddOperator(InfixOperator("*",  spaces, 7, Associativity.Left, fun x y -> BinaryExpression(x, Multiplication, y) |> AstExpression))
    opp.AddOperator(InfixOperator("/",  spaces, 7, Associativity.Left, fun x y -> BinaryExpression(x, Division, y) |> AstExpression))
    opp.AddOperator(InfixOperator("%",  spaces, 7, Associativity.Left, fun x y -> BinaryExpression(x, Remainder, y) |> AstExpression))

    opp.AddOperator(PrefixOperator("!", spaces, 8, true, fun x -> UnaryExpression(LogicalNegate, x) |> AstExpression)) 
    opp.AddOperator(PrefixOperator("-", spaces, 8, true, fun x -> UnaryExpression(Negate, x) |> AstExpression)) 

    opp.AddOperator(InfixOperator(".", lookAhead (spaces .>> pFunctionCall), 9, Associativity.Left,
                                 fun x y -> MemberExpression (MemberFunctionCall(x,y)) |> AstExpression))

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
        pListInitializerExpression |>> AstExpression
        attempt pStaticMemberExpression|>> AstExpression
        Literal.pLiteralExpression |>> AstExpression
        attempt pFunctionCallExpression |>> AstExpression
        pIdentifierExpression |>> AstExpression
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
                 let (AstExpression expr) = expr
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
        opt (Keyword.pExtends >>. Types.pQualifiedType |>> CustomTypeSpec ) .>>.
        (opt (Keyword.pImplements >>. (sepBy1 Types.pQualifiedType Char.comma |>> List.map CustomTypeSpec) ) >>= toList)
    
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
        let property = 
             pipe3
                (Keyword.pVal >>. pIdentifier)
                (Char.colon >>. Types.pTypeSpec)
                (opt (Char.equals >>. Expression.pExpression))
                (fun name t initializer -> { Name = name; Type = t; Initializer = initializer})
        tuple3
            (many property)
            (opt pConstructor)
            (many Function.pFunctionDeclaration)

    let pClass : Parser<Class<AstExpression>, unit> =
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
                Functions = functions;
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
let parseDeclarations = 
    parseProgram 
    >>
    function
    | ParserResult.Success(result, _, _) -> Result.succeed result 
    | ParserResult.Failure(message, error, state) -> Result.failure (SyntaxError ((message, error, state).ToString()))

let parseModules (source : SourceFile list) = 
    let buildModule (name, declarationsResult) = 
        declarationsResult 
        |> Result.map (fun decls -> Module.create name decls)
    source
    |> List.map (fun s -> (s.Name, parseDeclarations s.Code))
    |> List.map buildModule
    |> Result.merge
    