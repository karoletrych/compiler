module Compiler.IRGeneration
open IR
open Ast
open TypeInference
open AstProcessing
open Types

/// memory location type to which identifier is referring
type FieldDeclarationPlace =
| ThisType
| External of TypeIdentifier

type DataStorage =
| FieldRef of FieldDeclarationPlace
| Argument
| LocalVariable


/// instructions necessery to access given identifier
let loadFromIdentifier identifiers idType (id : string) =
    match identifiers |> Map.find id with
    | FieldRef place -> 
        match place with
        | ThisType -> [LdThis; Ldfld(id)]
        | External tId -> 
            let fieldRef = {FieldName = id; IsStatic = false; FieldType = idType}
            [GetExternalField(tId, fieldRef, [LdThis])]
    | Argument -> [Ldarg(id)]
    | LocalVariable -> [Ldloc(id)]
/// instructions necessery to store to given identifier
let storeToIdentifier identifiers idType (id : string) setter =
    match identifiers |> Map.find id with
    | FieldRef place -> 
        match place with
        | ThisType -> [LdThis] @ setter @ [Stfld(id)]
        | External tId -> 
            let fieldRef = {FieldName = id; IsStatic = false; FieldType = idType}
            [SetExternalField(tId, fieldRef, [LdThis], setter)]
    | Argument -> setter @ [Starg(id)]
    | LocalVariable -> setter @ [Stloc(id)]

/// takes mapping of identifiers to local data location, context
/// of current method and an expression
/// return list of instructions for a given expression
let rec private convertExpression identifiers context (expr : InferredTypeExpression) =
    let convertExpression = convertExpression identifiers context
    let (InferredTypeExpression(expr, expressionTypeIdentifier)) = expr
    match expr with
    | LiteralExpression l -> 
        match l with 
        | BoolLiteral b -> [ (if b then LdcI4(1) else LdcI4(0)) ]
        | IntLiteral i -> [ LdcI4(i) ]
        | FloatLiteral f -> [ LdcR4(f) ]
        | StringLiteral s -> [ Ldstr(s) ]
    | AssignmentExpression(assignee, expr) -> 
        match assignee with
        | MemberFieldAssignee (callee, i) ->
            failwith ""
        | IdentifierAssignee i -> 
                [Duplicate]
                @ storeToIdentifier identifiers expressionTypeIdentifier i (convertExpression expr)
    | BinaryExpression(e1, op, e2) ->
        let args = convertExpression e1 @ convertExpression e2 
        let t = getType e1
        match t with
        | t when t = Identifier.int || t = Identifier.float || t = Identifier.double || t = Identifier.bool-> 
            match op with
            | Equal -> args @ [Ceq]
            | NotEqual -> args @ [Ceq; LdcI4 0; Ceq]
            | LogicalOr -> args @ [Or]
            | LogicalAnd -> args @ [And]
            | LessEqual -> args @ [Clt; LdcI4 0; Ceq]
            | Less -> args @ [Clt]
            | GreaterEqual -> args @ [Cgt; LdcI4 0; Ceq]
            | Greater -> args @ [Cgt]
            | Plus -> args @ [Add]
            | Minus -> args @ [Sub]
            | Multiplication-> args @ [Mul]
            | Division -> args @ [Div]
            | Remainder -> args @ [Rem]
        | t when t = Identifier.string ->
            match op with
            | Plus -> [CallMethod(t,{ MethodName = "Concat"; 
                        Parameters = [t; t]; 
                        Context = Static},
                            [], 
                            args)]
            | _ -> [CallMethod(
                        t,
                        {
                            MethodName = operatorMethodName op; 
                            Parameters = [t; t]; 
                            Context = Static},
                            [], 
                            args)]
        | _ ->
            [CallMethod(t,
                {
                    MethodName = operatorMethodName op; 
                    Parameters = [t; t]; 
                    Context = Static},
                    [], 
                    args)]
    | InstanceMemberExpression(calleeExpression, mem) -> 
        let (InferredTypeExpression(callee, calleeT)) = calleeExpression
        match mem with
        | MemberFunctionCall call ->
            [CallMethod(
                        calleeT,
                        {
                            MethodName = call.Name
                            Parameters = call.Arguments |> List.map getType
                            Context = Instance
                        },
                        convertExpression calleeExpression,
                        (call.Arguments |> List.collect convertExpression)
                        )
                        ]
        | MemberField(fieldName) -> 
             [GetExternalField(calleeT, {FieldName = fieldName; IsStatic = false; FieldType = expressionTypeIdentifier}, convertExpression calleeExpression)]
    | IdentifierExpression(identifier) -> loadFromIdentifier identifiers expressionTypeIdentifier identifier 
    | ListInitializerExpression list ->
        let add param = {
            MethodName = "Add"
            Parameters = [getType param]
            Context = Instance
        }
        [NewObj(expressionTypeIdentifier, [])]
      @ (list 
         |> List.collect (fun item -> 
                            [Duplicate] 
                          @ convertExpression item 
                          @ [CallMethod(expressionTypeIdentifier, add item, [], [])])
        )
    | NewExpression(t, args) -> 
        (args |> List.collect convertExpression)
      @ [NewObj(Identifier.typeId t, args |> List.map getType)]
    | StaticMemberExpression(t, m) -> 
        match m with
        | MemberFunctionCall call ->
            let args = call.Arguments |> List.collect convertExpression
            [CallMethod(
                Identifier.typeId t, 
                { MethodName = call.Name; Parameters = call.Arguments |> List.map getType; Context = Static},
                [],
                args)]
        | MemberField f ->
              [GetExternalField(Identifier.typeId t, { FieldName = f; IsStatic = true; FieldType = expressionTypeIdentifier}, []);]
    | UnaryExpression(op, e) -> 
        match op with
        | Negate -> convertExpression e @ [Neg]
        | LogicalNegate -> convertExpression e @ [LdcI4 0; Ceq]
    | LocalFunctionCallExpression(lfc) -> 
        [CallLocalMethod ({
                            MethodName = lfc.Name
                            Parameters = lfc.Arguments |> List.map getType
                            Context = context
                           },
                           (match context with
                            | Static -> [] 
                            | Instance ->  [LdThis]),
                           (lfc.Arguments |> List.collect convertExpression))
            ]

/// unique id of label
let mutable label = 0
/// generate next label id
let nextLabelId () = 
    label <- label + 1
    label

let isRet = 
    function
    | Ret _ -> true
    | _ -> false

let isLabel = 
    function
    | Label _ -> true
    | _ -> false

let retExists instructions = 
        instructions 
           |> List.exists isRet
let retIsLast instructions = 
    instructions 
    // |> List.findBack (isLabel >> not)
    |> List.last
    |> isRet

    


/// converts statements to IR instructions
let rec private convertStatements types context identifiers statements : Instruction list =
    let convertExpression = convertExpression identifiers context
    let rec generateIRFromStatement (statement : Statement<InferredTypeExpression>) =
        match statement with
        | StaticFunctionCallStatement (t, method) -> 
            let typeId = Identifier.typeId t
            let methodRef = {MethodName = method.Name; Parameters = method.Arguments |> List.map getType; Context = Static}
            [CallMethod(typeId, methodRef, [], (method.Arguments |> List.collect convertExpression))]
        | AssignmentStatement(assignee, setterExpression) -> 
            match assignee with
            | MemberFieldAssignee (assignee, fieldName) -> 
                    let declaringType = 
                        (
                        types 
                        |> Map.find (getType assignee)
                        |> (fun t -> t.Fields |> List.find (fun f -> f.FieldName = fieldName))
                        ).FieldDeclaringType
                    let fieldRef = {FieldName = fieldName; IsStatic = false; FieldType = getType setterExpression}
                    [SetExternalField(declaringType, fieldRef, convertExpression assignee, convertExpression setterExpression)]
            | IdentifierAssignee assignee -> 
                    storeToIdentifier identifiers (getType setterExpression) assignee (convertExpression setterExpression)
        | CompositeStatement(cs) -> cs |> List.collect generateIRFromStatement
        | LocalFunctionCallStatement(lfc) -> 
            [CallLocalMethod 
                        ({
                            MethodName = lfc.Name
                            Parameters = lfc.Arguments |> List.map getType
                            Context = context
                           }, 
                           (match context with
                            | Static -> [] 
                            | Instance ->  [LdThis]), 
                           (lfc.Arguments |> List.collect convertExpression))]
        | IfStatement(condition, statement, elseStatement) ->
            let elseLabel = nextLabelId() // wygenerowanie etykiety dla początku else
            let endLabel = nextLabelId()  // wygenerowanie etykiety dla końca if'a
            
            // transformacja instrukcji z opcjonalnego bloku else 
            // do listy instrukcji reprezentacji pośredniej
            let elseStatements =          
                elseStatement
                |> Option.map generateIRFromStatement
                |> Option.toList
                |> List.concat

            // zwracane są skonkatenowane następujące instrukcje:
            convertExpression condition  // instrukcje warunku if'a
            @ [Brfalse elseLabel]        // jeśli warunek nieprawdziwy skocz do else
            @ generateIRFromStatement statement  // instrukcje z właściwego bloku kodu
            @ [Br endLabel]              // skok do końca if'a
            @ [Label elseLabel]          // etykieta początku bloku else
            @ elseStatements             // instrukcje z bloku else
            @ [Label endLabel]           // etykieta końca instrukcji if

        | InstanceMemberFunctionCallStatement(calleeExpression, call) ->
            [CallMethod(
                getType calleeExpression,
                        {
                            MethodName = call.Name
                            Parameters = call.Arguments |> List.map getType
                            Context = Instance
                        }, convertExpression calleeExpression, call.Arguments |> List.collect convertExpression)
                        ]
        | ReturnStatement(r) -> 
            match r with 
            | Some r -> convertExpression r @ [Ret (Some (getType r))]
            | None -> [Ret None]
        | VariableDeclaration(vd) -> 
            match vd with
            | DeclarationWithInitialization (name, init) -> 
                convertExpression init 
              @ [Stloc(name)]
            | DeclarationWithType (name, _) -> 
                [Stloc(name)]
            | FullDeclaration (name, _, init) -> 
                convertExpression init 
                @ [Stloc(name)]
        | ValueDeclaration(name, _, init) ->
            convertExpression init 
            @ [Stloc(name)]
        | WhileStatement(expr, s) -> 
            let endLabel = nextLabelId()
            let beginLabel = nextLabelId()
            [Label beginLabel]
            @ convertExpression expr 
            @ [Brfalse endLabel]
            @ generateIRFromStatement s 
            @ [Br beginLabel] 
            @ [Label endLabel]

    let instructions = 
        statements |> generateIRFromStatement
    
    instructions 
      @ if not (retExists instructions) || not (retIsLast instructions)
        then [Ret None] 
        else []

/// finds all variable declarations in a method body
let findLocalVariables body : Variable list =
    let idFold acc _ = acc
    let valueDeclaration acc (name, _, expr) = {Name = name; TypeId = getType expr} :: acc
    let declarationWithInitialization acc (name,expr) = {Name =name; TypeId = getType expr} :: acc
    let declarationWithType acc (name, t) = {Name = name; TypeId = Identifier.typeId t} :: acc
    let fullVariableDeclaration acc (name, _, expr) = {Name = name; TypeId = getType expr} :: acc
    let ifStatement (stmt, elseStmt) = 
        stmt @ (elseStmt |> Option.defaultValue []) 

    let variables = 
        statementFold idFold idFold 
            valueDeclaration 
            declarationWithInitialization 
            declarationWithType 
            fullVariableDeclaration 
            idFold idFold idFold idFold idFold ifStatement
                [] (CompositeStatement body)
    variables

/// converts AST function to its IR counterpart
let private buildFunction types context fields (func : Function<InferredTypeExpression>) : IR.Function = 
    let localVariables = findLocalVariables func.Body
    let identifiers = 
        (localVariables |> List.map (fun v -> v.Name, LocalVariable))
        @ (func.Parameters |> List.map (fun p -> fst p, Argument))
        @ fields
        |> Map.ofList
    {
        Name = func.Name
        Body = convertStatements types context identifiers (CompositeStatement func.Body)
        ReturnType = Identifier.typeId func.ReturnType.Value
        Parameters = 
            func.Parameters 
            |> List.map (fun p -> {Name = fst p; TypeId = Identifier.typeId (snd p) })
        LocalVariables = localVariables
        Context = context
    }

/// converts field to IR.Variable
let private buildField (prop : Ast.Field<InferredTypeExpression>) = {
    Name = prop.Name;
    TypeId = prop.Type |> Identifier.typeId
}

/// returns instructions necessary to initalize fields
let fieldInitializers identifiers fields = 
    fields
    |> List.collect (fun f -> 
            f.Initializer 
            |> Option.map (fun initializer -> 
                    [LdThis] @ (convertExpression identifiers Instance initializer) @ [Stfld f.Name]
                )
            |> function
               | Some o -> o
               | None -> []
        )

/// converts AST constructor to its IR counterpart
let private buildConstructor types classFields fields baseType (ctor : Constructor<InferredTypeExpression>) : IR.Constructor = 
    let baseArgTypes = 
        ctor.BaseClassConstructorCall
        |> List.map getType
    let localVariables = findLocalVariables ctor.Body
    let identifiers = 
        (localVariables |> List.map (fun v -> v.Name, LocalVariable))
      @ (ctor.Parameters |> List.map (fun p -> fst p, Argument))
      @ fields
      |> Map.ofList
    let baseInitialiserArgs =
        ctor.BaseClassConstructorCall 
        |> List.collect (convertExpression identifiers Static)
    { 
        Parameters = ctor.Parameters 
                     |> List.map (fun p -> { Name = fst p; TypeId = Identifier.typeId (snd p) })
        Body = [LdThis]
               @ baseInitialiserArgs
               @ [CallConstructor(baseType, baseArgTypes)]
               @ convertStatements types Static identifiers (CompositeStatement ctor.Body)
               @ (classFields |> fieldInitializers identifiers)
        LocalVariables = findLocalVariables ctor.Body
    }

/// generates instructions for default constructor
/// calling base class constructor basically
let buildDefaultConstructor classFields fields baseType =
    let identifiers = 
        fields    
        |> Map.ofList
    let instructions = 
        [LdThis; CallConstructor(baseType, [])] @ fieldInitializers identifiers classFields
    { 
        Parameters = []
        Body = instructions @ if not (retExists instructions) then [Ret None] else []
        LocalVariables = []
    }


/// converts class to its IR counterpart
let private buildClass (types : Map<TypeIdentifier, Types.Type>) (clas : Ast.ModuleClass<InferredTypeExpression>)  : IR.Class = 
    let baseType = clas.BaseClass |> Option.map Identifier.typeId |> Option.defaultValue Identifier.object
    let fields = 
        (clas.Fields |> List.map (fun f -> (f.Name, FieldRef ThisType)))
      @ (types.[baseType].Fields |> List.map(fun f -> f.FieldName, FieldRef(External baseType)))
    {
        Identifier = clas.Identifier
        Methods = clas.Functions |> List.map (buildFunction types Instance fields)
        Fields = clas.Fields |> List.map buildField
        BaseClass = baseType
        Constructors = 
            match clas.Constructors with
            | [] -> [buildDefaultConstructor clas.Fields fields baseType]
            | ctors -> ctors |> List.map (buildConstructor types clas.Fields fields baseType)
    }

/// converts module to its IR counterpart
let private buildModule types (modul : Module<InferredTypeExpression>) : IR.Module = {
    Identifier = modul.Identifier
    Classes = modul.Classes |> List.map (buildClass types)
    Functions = modul.Functions |> List.map (buildFunction types Static [])
} 
/// generates IR.Modules from AST.Modules
let generateIR (modules, types) : IR.Module list =
    modules |> List.map (buildModule types)
