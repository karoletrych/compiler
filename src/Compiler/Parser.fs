module Compiler.Parser

open System
open System.IO
open Ast
open FParsec
open System.Text.RegularExpressions

type SourceFile = {
    Path : string 
    Code : string
}

let removeComments input =  
    let blockComments = @"/\*(.*?)\*/";
    let lineComments = @"//(.*?)\r?\n";
    Regex.Replace(input, blockComments + "|" + lineComments, 
        (fun (me : Match) -> 
            if me.Value.StartsWith(@"//") 
            then Environment.NewLine 
            else ""),
            RegexOptions.Multiline)

/// maps optional list into parser returning elements of this list or empty list if none
let toList = 
    function
    | Some (args) -> preturn args
    | None -> preturn [] 

/// terminal symbol parsers. Consume single symbol and whitespaces after it.
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
    /// consumes nonalphanumeric characters
    let nonAlphanumericWs = nextCharSatisfiesNot (isAsciiLetter) >>. spaces

    let pFun = skipString "fun" .>> nonAlphanumericWs
    let pReturn = skipString "return" .>> nonAlphanumericWs
    let pVar = skipString "var" .>> nonAlphanumericWs |>> (fun () -> false)
    let pVal = skipString "val" .>> nonAlphanumericWs |>> (fun () -> true)
    let pIf = skipString "if" .>> nonAlphanumericWs
    let pElse = skipString "else" .>> nonAlphanumericWs
    let pClass = skipString "class" .>> nonAlphanumericWs
    let pConstructor = skipString "construct" .>> nonAlphanumericWs
    let pNew = skipString "new" .>> nonAlphanumericWs
    let pImplements = skipString "implements" .>> nonAlphanumericWs
    let pExtends = skipString "extends" .>> nonAlphanumericWs
    let pModule = skipString "module" .>> nonAlphanumericWs
    let pWhile = skipString "while" .>> nonAlphanumericWs

    /// list of above keywords. Used for checking restricted identifiers
    let keywords = [ 
        "fun"
        "return"
        "var"
        "val"
        "if"
        "else"
        "class"
        "construct"
        "new"
        "true"
        "false"
        "module"
        "while"
    ]

/// Identifier parser and reference to its implementation. 
/// Used for recursive implementation of pIdentifier i.e. pIdentifier is defined in terms of itself.
let pIdentifier, pIdentifierImpl = createParserForwardedToRef<string, _>() 

module Types =
    let pTypeSpec, pTypeSpecImpl = createParserForwardedToRef()
    let pNonGenericTypeSpec = pIdentifier 
    /// <T (,V)* >
    let pGenericArguments =  between Char.leftAngleBracket Char.rightAngleBracket (sepBy1 pTypeSpec Char.comma)

    /// generic type spec consists of nonGenericTypeSpec followed by arguments. 
    let pGenericType =
        pNonGenericTypeSpec .>>. pGenericArguments
        |>> (fun (name, types) -> {Name = name; GenericArgs = types})
        /// ^ build appropriate AST record out of generic type spec parts 
    let pNonGenericType = 
        pNonGenericTypeSpec 
        |>> fun t -> ({Name = t; GenericArgs = []})

    /// validates TypeSpec consisting of namespace and type name at the end
    /// fails if it contains generic segment not at the last position f. e.
    /// Foo::Bar<string>::End 
    let convertToFullyQualifiedType =
        let rec segments acc types =
            match types with
            | [lastTypeSpec] -> preturn (acc, lastTypeSpec)
            | head :: tail -> 
                match head with
                | {Name = identifier; GenericArgs = []}
                    -> segments (acc @ [identifier]) tail 
                | _
                    -> fail "No generic type is allowed as namespace segment"
            | [] -> fail "Should not happen"
        segments [] 

    /// type consisting of namespace segments and type name separated by ::
    /// it consumes either generic and nongeneric parts and validates it later in "convertToFullyQualifiedType"
    /// it is built this way because this construction A::B::C::D is left recursive
    let pQualifiedType = 
            sepBy (attempt pGenericType <|> pNonGenericType) Char.doubleColon
                >>= convertToFullyQualifiedType
    let pCustomType = pQualifiedType |>> CustomTypeSpec

    /// dictionary of parsers of builtin typespecs. Parsers return appropriate AST terminal nodes.
    let builtInTypesParsersDict =
        [ 
            ("bool", stringReturn "bool" Bool);
            ("int", stringReturn "int" Int);
            ("float", stringReturn "float" Float);
            ("string", stringReturn "string" String);
            ("void", stringReturn "void" Void);
        ]
        |> Map.ofList
    let builtInTypeSpecParsers =
        builtInTypesParsersDict |> Map.toList |> List.map (snd >> fun p -> p |>> fun bits -> bits |> BuiltInTypeSpec)
    
    /// assignment of implementation of typeSpec parser
    pTypeSpecImpl := 
        choice(attempt pCustomType :: builtInTypeSpecParsers) .>> spaces

/// identifier can start from _ and can contain it inside
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

let builtInTypesKeywords = Types.builtInTypesParsersDict |> Map.toSeq |> Seq.map fst

/// identifier is not allowed to be a keyword nor to be called like builtin type spec
pIdentifierImpl := identifier .>> spaces
>>= (fun id -> 
    if not (List.contains id Keyword.keywords)
         && not (builtInTypesKeywords |> Seq.contains (id))
    then preturn id 
    else fail ("Identifier cannot be a keyword: " + id))

module Expression = 
    /// Parser for expressions. 
    /// Parser combinators like in other places cannot be used due to expression being left recursive.
    let opp = new OperatorPrecedenceParser<AstExpression,_,_>()
    let pExpression = opp.ExpressionParser
    let pArgumentList = sepBy pExpression Char.comma
    let pFunctionCall = 
        pipe3
            pIdentifier
            (opt Types.pGenericArguments >>= toList)
            (between Char.leftParen Char.rightParen pArgumentList)
            (fun name genericArgs args -> {Name = name; GenericArguments = genericArgs; Arguments = args})

    let pFunctionCallExpression = 
        pFunctionCall 
        |>> LocalFunctionCallExpression
        |>> AstExpression
    module Literal =
        /// string literal consists of characters not being quotation mark surrounded by quotation marks
        let pStringLiteral = 
            (between (pstring "\"") 
                (pstring "\"") 
                (manySatisfy ((<>) '\"')) )
                .>> spaces
            |>> StringLiteral

        let pFloatLiteral = pfloat |>> (single >> FloatLiteral)
        let pIntLiteral = (pint32 .>> notFollowedBy (pstring ".")) .>> spaces |>> IntLiteral
        let pBoolParser = (pstring "true" >>% BoolLiteral true) <|> (pstring "false" >>% BoolLiteral false)
        let pLiteralExpression = choice [pBoolParser; attempt pIntLiteral; pFloatLiteral; pStringLiteral] |>> LiteralExpression
        
    let pParenthesizedExpression = 
        between Char.leftParen Char.rightParen pExpression
    let pIdentifierExpression = 
        pIdentifier 
            |>> fun id -> IdentifierExpression(id)
            |>> AstExpression

    /// out of any two expressions assumes that the first on is an identifier or member field
    /// throws exception when it is not
    /// creates assignment expression out of the assignee and expression being assigned
    let createAssignmentExpr x y = 
        let (AstExpression e) = x
        let x = 
            match e with
            | IdentifierExpression x -> IdentifierAssignee x
            | InstanceMemberExpression (e,i) -> 
                match i with
                | MemberField i -> MemberFieldAssignee (e, i)
                | MemberFunctionCall _ -> failwith "assignee have to be either identifier or member field"
            | _ -> failwith "assignee have to be either identifier or member field"
        AssignmentExpression(x,y)

    opp.AddOperator(PrefixOperator("!", spaces, 8, true, fun x -> UnaryExpression(LogicalNegate, x) |> AstExpression)) 
    opp.AddOperator(PrefixOperator("-", spaces, 8, true, fun x -> UnaryExpression(Negate, x) |> AstExpression)) 

    opp.AddOperator(InfixOperator("*",  spaces, 7, Associativity.Left, fun x y -> BinaryExpression(x, Multiplication, y) |> AstExpression))
    opp.AddOperator(InfixOperator("/",  spaces, 7, Associativity.Left, fun x y -> BinaryExpression(x, Division, y) |> AstExpression))
    opp.AddOperator(InfixOperator("%",  spaces, 7, Associativity.Left, fun x y -> BinaryExpression(x, Remainder, y) |> AstExpression))

    opp.AddOperator(InfixOperator("+",  spaces, 6, Associativity.Left, fun x y -> AstExpression(BinaryExpression(x, Plus, y))))
    opp.AddOperator(InfixOperator("-",  spaces, 6, Associativity.Left, fun x y -> BinaryExpression(x, Minus, y) |> AstExpression))

    opp.AddOperator(InfixOperator("<=", spaces, 5, Associativity.None, fun x y -> BinaryExpression(x, LessEqual, y) |> AstExpression))
    opp.AddOperator(InfixOperator(">=", spaces, 5, Associativity.None, fun x y -> BinaryExpression(x, GreaterEqual, y) |> AstExpression))
    opp.AddOperator(InfixOperator(">",  spaces, 5, Associativity.None, fun x y -> BinaryExpression(x, Greater, y) |> AstExpression))
    opp.AddOperator(InfixOperator("<",  spaces, 5, Associativity.None, fun x y -> BinaryExpression(x, Less, y) |> AstExpression))

    opp.AddOperator(InfixOperator("==", spaces, 4, Associativity.Left, fun x y -> BinaryExpression(x, Equal, y) |> AstExpression))
    opp.AddOperator(InfixOperator("!=", spaces, 4, Associativity.Left, fun x y -> BinaryExpression(x, NotEqual, y) |> AstExpression))

    opp.AddOperator(InfixOperator("&&", spaces, 3, Associativity.Left, fun x y -> BinaryExpression(x, LogicalAnd, y) |> AstExpression))
    opp.AddOperator(InfixOperator("||", spaces, 2, Associativity.Left, fun x y -> BinaryExpression(x, LogicalOr, y) |> AstExpression))

    opp.AddOperator(InfixOperator("=", spaces, 1, Associativity.Right, fun x y -> createAssignmentExpr x y |> AstExpression))

    /// assumes second expression is either member field or member function call
    /// creates MemberExpression out of first expression and above member
    let createMemberExpr x y = 
        let (AstExpression e) = y
        match e with
        | LocalFunctionCallExpression x -> MemberFunctionCall x
        | IdentifierExpression x -> MemberField x
        | _ -> failwith "parser internal error"
        |> fun instanceMember -> InstanceMemberExpression(x, instanceMember)

    let instanceMemberExpr = attempt (pFunctionCallExpression) <|> (pIdentifierExpression)

    opp.AddOperator(
        InfixOperator(
            ".", 
            lookAhead (spaces .>> instanceMemberExpr), /// parses only these expressions where part after dot is functioncall or identifier
            9,
            Associativity.Left,
            fun x y -> (createMemberExpr x y) |> AstExpression)
            )

    let pNewExpression = 
        Keyword.pNew >>. Types.pTypeSpec .>>. between Char.leftParen Char.rightParen pArgumentList |>> NewExpression

    let pStaticFunctionCall =
        Char.colonDot >>. pFunctionCall
        |>> MemberFunctionCall

    let pStaticField =
        Char.colonDot >>. pIdentifier
        |>> MemberField
    
    let staticMember = 
        Types.pCustomType .>>. ((attempt pStaticFunctionCall) <|> pStaticField)
        |>> StaticMemberExpression

    let pListInitializerExpression =
             between 
                 Char.leftSquareBracket
                 Char.rightSquareBracket 
                 (sepBy pExpression Char.semicolon) |>> ListInitializerExpression

    opp.TermParser <- choice [
        pListInitializerExpression |>> AstExpression
        attempt staticMember       |>> AstExpression
        Literal.pLiteralExpression |>> AstExpression
        pNewExpression |>> AstExpression
        attempt pFunctionCallExpression
        pIdentifierExpression
        pParenthesizedExpression 
    ] 

module Statement =
    let pStatement, pStatementRef = createParserForwardedToRef()

    let pElseStatement = Keyword.pElse >>. pStatement
    let pIfStatement = 
        pipe3 
            (Keyword.pIf >>. Expression.pExpression) 
            (pStatement) 
            (opt pElseStatement)
            (fun expression statement elseSt -> IfStatement(expression, statement, elseSt))
    let pWhileStatement = 
            (Keyword.pWhile >>. Expression.pExpression) .>>. pStatement
            |>> fun (expression, statement) -> WhileStatement(expression, statement)

    let pFunctionCallStatement = Expression.pFunctionCall |>>  LocalFunctionCallStatement
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
                       | (name, Some t, Some initializer) -> preturn (FullDeclaration(name, t, initializer))
                       | (name, Some t, None) -> preturn (DeclarationWithType(name, t))
                       | (name, None, Some initializer) -> preturn (DeclarationWithInitialization(name, initializer))
                       | (_, None, None) -> fail "Implicitly typed variable must be initialized")
        )  
    let pLocalValueDeclarationStatement = 
        tuple3
            (Keyword.pVal >>. pIdentifier)
            (opt (Char.colon >>. Types.pTypeSpec))
            (Char.equals >>. Expression.pExpression)

    /// parser of expression which can also be statements (function call, assignment, local function call)
    let expressionStatement =
        Expression.pExpression
        >>= (fun expr ->
                 let (AstExpression expr) = expr
                 match expr with
                 | InstanceMemberExpression(callee, memberFunction) -> 
                            match memberFunction with
                            | MemberFunctionCall functionCall -> preturn (InstanceMemberFunctionCallStatement (callee, functionCall))
                            | MemberField _ -> fail "given expression cannot be a statement"
                 | AssignmentExpression(assignee,e) -> preturn (AssignmentStatement (assignee, e))
                 | LocalFunctionCallExpression fc -> preturn (LocalFunctionCallStatement (fc))
                 | _ -> fail "given expression cannot be a statement" )

    let pStaticFunctionCallStatement =
        attempt Types.pCustomType .>>.
        (Char.colonDot >>. Expression.pFunctionCall)
        |>> StaticFunctionCallStatement

    /// statement consisting of another statements (block of code)
    let compositeStatement = 
        between 
            Char.leftBrace Char.rightBrace
            (many pStatement)
        |>> CompositeStatement

    pStatementRef := 
        choice
            [
                compositeStatement;
                pIfStatement;
                pWhileStatement;
                (pLocalValueDeclarationStatement .>> Char.semicolon) |>> ValueDeclaration;
                (pLocalVariableDeclarationStatement .>> Char.semicolon) |>> VariableDeclaration
                pReturnStatement .>> Char.semicolon;
                attempt pStaticFunctionCallStatement .>> Char.semicolon;
                attempt expressionStatement .>> Char.semicolon;
                attempt pFunctionCallStatement .>> Char.semicolon;
            ]
    
module Function = 
    let pFunctionDeclaration = 
        let parameter =
            Char.leftParen >>. pIdentifier  .>>. (Char.colon >>. Types.pTypeSpec) .>> Char.rightParen 

        let parametersList =
            many parameter
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
            (fun name parameters returnType body 
                -> {
                    Name = name;
                    Parameters = parameters;
                    ReturnType = returnType;
                    Body = body
                }
            )


module Class =
    let pClassName = Types.pNonGenericTypeSpec
    let pInheritanceDeclaration = 
        opt (Char.colon >>. Types.pQualifiedType |>> CustomTypeSpec) 
    
    let pClassBody = 
        let pConstructor = // construct(a : int, b : float) : (a, b)
            let parameter =
                Char.leftParen >>. pIdentifier  .>>. (Char.colon >>. Types.pTypeSpec) .>> Char.rightParen 

            let parametersList =
                many parameter
            let pBaseCall =  
                opt (Char.colon >>. (between Char.leftParen Char.rightParen 
                        (sepBy1 Expression.pExpression Char.comma))) 
                    >>= toList
            pipe3 
                (Keyword.pConstructor >>. parametersList)
                pBaseCall
                (between Char.leftBrace Char.rightBrace (many Statement.pStatement))
                (fun pars baseCall body -> { Parameters = pars; BaseClassConstructorCall = baseCall; Body = body})
        let field = 
             pipe4
                (Keyword.pVar <|> Keyword.pVal)
                pIdentifier
                (Char.colon >>. Types.pTypeSpec)
                (opt (Char.equals >>. Expression.pExpression))
                (fun readonly name t initializer -> { IsReadOnly = readonly; Name = name; Type = t; Initializer = initializer})
        tuple3
            (many field)
            (many pConstructor)
            (many Function.pFunctionDeclaration)

    let pClass : Parser<Class<AstExpression>, unit> =
        pipe3
            (Keyword.pClass >>. pClassName)
            pInheritanceDeclaration
            (between Char.leftBrace Char.rightBrace pClassBody)
            (fun name inheritanceDeclaration body ->
            let fields, constructors, functions = body
            {
                Name = name;
                BaseClass = inheritanceDeclaration;
                Fields = fields;
                Constructors = constructors;
                Functions = functions;
            })

let pDeclaration = 
    choice[
        Function.pFunctionDeclaration |>> FunctionDeclaration
        Class.pClass |>> ClassDeclaration
        ]
let moduleIdentifier = opt (Keyword.pModule >>. sepBy1 pIdentifier Char.doubleColon)

/// source file consists of optional module identifier ("module A::B") followed by class and function declarations
let pProgramFile = 
    spaces >>. moduleIdentifier .>>. many pDeclaration 
    |>> (fun (mi,decls) -> {ModuleIdentifier = mi; Declarations = decls})

let parseProgramFile =
    removeComments >> run pProgramFile

open CompilerResult
let parse = 
    parseProgramFile 
    >>
    function
    /// FParsec parsing result is mapped into CompilerResult
    | ParserResult.Success(result, _, _) -> Result.succeed result 
    | ParserResult.Failure(message, error, state) -> Result.failure (SyntaxError ((message, error, state).ToString()))

let parseModules (source : SourceFile list) = 
    let buildModule (fileName : string, programFile : CompilerResult<ProgramFile>) = 
        programFile 
        |> Result.map (fun program ->
            let moduleId = 
                /// depending on whether module identifier was specified gives module 
                /// either the specified moduleId or one based on path
                match program.ModuleIdentifier with
                | None -> fileName.Split(Path.DirectorySeparatorChar) |> List.ofArray
                | Some s -> s
            Module.create moduleId program.Declarations)
    source
    |> List.map ((fun s -> (s.Path, parse s.Code)) >> buildModule)
    |> Result.merge
    
