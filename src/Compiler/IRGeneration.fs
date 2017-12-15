module Compiler.IRGeneration
open IR
open Ast
open TypeInference
open AstProcessing
open Compiler.Types

type DataStorage =
| Field
| Argument
| LocalVariable

let loadFromIdentifier identifiers (id : string) =
    match identifiers |> Map.find id with
    | Field -> [LdargIdx 0s; Ldfld(id)]
    | Argument -> [Ldarg(id)]
    | LocalVariable -> [Ldloc(id)]
let storeToIdentifier identifiers (id : string) =
    match identifiers |> Map.find id with
    | Field -> Stfld(id)
    | Argument -> Starg(id)
    | LocalVariable -> Stloc(id)

let rec private convertExpression identifiers context (expr : InferredTypeExpression) =
    let convertExpression = convertExpression identifiers context
    let (InferredTypeExpression(expr, exprType)) = expr
    match expr with
    | LiteralExpression l -> 
        match l with 
        | BoolLiteral b -> [ (if b then LdcI4(1) else LdcI4(0)) ]
        | IntLiteral i -> [ LdcI4(i) ]
        | FloatLiteral f -> [ LdcR4(f) ]
        | StringLiteral s -> [Ldstr(s)]
    | AssignmentExpression(assignee, expr) -> 
        match assignee with
        | MemberFieldAssignee (callee, i) -> failwith "TODO:"
        | IdentifierAssignee i -> 
            match storeToIdentifier identifiers i with
            | Stfld f -> 
                [LdargIdx 0s] @ convertExpression expr @ [Stfld f]
            | _ -> 
            convertExpression expr
            @ [Duplicate]
            @ [storeToIdentifier identifiers i]
    | BinaryExpression(e1, op, e2) ->
       match op with
        | ConditionalOr -> failwith "TODO:"
        | ConditionalAnd -> failwith  "TODO:"
        | Equal -> 
            let t = getType e1
            match t with
            | i when i = Identifier.int -> [Ceq]
            | s when s = Identifier.string -> 
                [CallMethod(Identifier.string, 
                    {MethodName = "op_Equality"; 
                    Parameters = [Identifier.string; Identifier.string]; 
                    Context = Static},
                    [], 
                    convertExpression e1 @ convertExpression e2)]
        | NotEqual -> failwith "TODO:"
        | LessEqual -> [Cle]
        | Less -> [Clt]
        | GreaterEqual -> [Cge]
        | Greater -> [Cge]
        | Plus -> [Add]
        | Minus -> [Sub]
        | Multiplication-> [Mul]
        | Division -> [Div]
        | Remainder -> [Rem]
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
            convertExpression calleeExpression @ [GetExternalField(calleeT, {FieldName = fieldName; IsStatic = false})]
    | IdentifierExpression(i) -> loadFromIdentifier identifiers i
    | ListInitializerExpression(list) ->
        let add param = {
            MethodName = "Add"
            Parameters = [getType param]
            Context = Instance
            }
        [NewObj(exprType, [])]
      @ (list |> List.collect (fun elem -> [Duplicate] @ convertExpression elem @ [CallMethod(exprType, add elem, [], [])]))
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
              [GetExternalField(Identifier.typeId t, { FieldName = f; IsStatic = true } )]
    | UnaryExpression(_, _) -> failwith "Not Implemented"
    | LocalFunctionCallExpression(lfc) -> 
        [CallLocalMethod ({
                            MethodName = lfc.Name
                            Parameters = lfc.Arguments |> List.map getType
                            Context = context
                           },
                           [],
                           (lfc.Arguments |> List.collect convertExpression))
            ]

let random = System.Random()
let randomInt () = random.Next()
let noRetInstruction instructions = 
        not (instructions 
           |> List.exists (function
                          | Ret -> true
                          | RetValue _ -> true
                          | _ -> false))
let rec private convertStatements isStatic identifiers statements : ILInstruction list =
    let convertExpression = convertExpression identifiers isStatic
    let rec generateIR (s : Statement<InferredTypeExpression>) =
        match s with
        | StaticFunctionCallStatement (t, method) -> 
            let typeId = (Identifier.typeId t)
            [CallMethod(typeId, {MethodName = method.Name; Parameters = method.Arguments |> List.map getType; Context = Static},[], (method.Arguments |> List.collect convertExpression))]
        | AssignmentStatement(assignee, expr) -> 
            match assignee with
            | MemberFieldAssignee (callee, i) -> failwith "TODO:"
            | IdentifierAssignee i -> 
                match storeToIdentifier identifiers i with
                | Stfld f -> 
                    [LdargIdx 0s] @ convertExpression expr @ [Stfld f]
                | _ ->
                    convertExpression expr
                    @ [storeToIdentifier identifiers i]
        | BreakStatement -> failwith "Not Implemented"
        | CompositeStatement(cs) -> cs |> List.collect generateIR
        | LocalFunctionCallStatement(lfc) -> 
            [CallLocalMethod ({
                            MethodName = lfc.Name
                            Parameters = lfc.Arguments |> List.map getType
                            Context = Static
                           }, [LdargIdx 0s], (lfc.Arguments |> List.collect convertExpression))]
        | IfStatement(expr, s, elseS) ->
            let elseLabel = randomInt()
            let elseStatements = 
                elseS 
                |> Option.map generateIR
                |> Option.toList
                |> List.concat

            convertExpression expr 
            @ [Brfalse elseLabel]
            @ generateIR s 
            @ [Label elseLabel]
            @ elseStatements

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
            | Some r -> convertExpression r @ [RetValue (getType r)]
            | None -> [Ret]
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
            let endLabel = randomInt()
            let beginLabel = randomInt()
            [Label beginLabel]
            @ convertExpression expr 
            @ [Brfalse endLabel]
            @ generateIR s 
            @ [Br beginLabel] 
            @ [Label endLabel]

    let instructions = 
        statements |> generateIR
    
    instructions @ if noRetInstruction instructions then [Ret] else []

let private findLocalVariables body : Variable list =
    let idFold acc _ = acc
    let valueDeclaration acc (name, _, expr) = {Name = name; Type = getType expr} :: acc
    let declarationWithInitialization acc (name,expr) = {Name =name; Type = getType expr} :: acc
    let declarationWithType acc (name, t) = {Name = name; Type = Identifier.typeId t} :: acc
    let fullVariableDeclaration acc (name, _, expr) = {Name = name; Type = getType expr} :: acc

    let variables = 
        statementFold idFold idFold 
            valueDeclaration 
            declarationWithInitialization 
            declarationWithType 
            fullVariableDeclaration 
            id idFold idFold id idFold idFold idFold
                [] (CompositeStatement body)
    variables

let private buildFunction context fields (func : Function<InferredTypeExpression>) : IR.Method = 
    let localVariables = findLocalVariables func.Body
    let identifiers = 
        (localVariables |> List.map (fun v -> v.Name, LocalVariable))
        @ (func.Parameters |> List.map (fun p -> fst p, Argument))
        @ (fields |> List.map (fun f -> (f, Field)))
        |> Map.ofList
    {
        Name = func.Name
        Body = convertStatements context identifiers (CompositeStatement func.Body)
        ReturnType = Identifier.typeId func.ReturnType.Value
        Parameters = 
            func.Parameters 
            |> List.map (fun p -> {Name = fst p; Type = Identifier.typeId (snd p) })
        LocalVariables = localVariables
        Context = context
    }

let private buildField (prop : Ast.Field<InferredTypeExpression>) = {
    Name = prop.Name;
    Type = prop.Type |> Identifier.typeId
}

let fieldInitializers identifiers = 
        List.collect (fun f -> 
                f.Initializer 
                |> Option.map (fun initializer -> 
                        [LdargIdx 0s] @ (convertExpression identifiers Instance initializer) @ [Stfld f.Name]
                    )
                |> function
                   | Some o -> o
                   | None -> []
            )

let private buildConstructor (fields : Field<InferredTypeExpression> list) baseType (ctor : Constructor<InferredTypeExpression>) : IR.Constructor = 
    let baseArgTypes = 
        ctor.BaseClassConstructorCall
        |> List.map getType
    let localVariables = findLocalVariables ctor.Body
    let identifiers = 
        (localVariables |> List.map (fun v -> v.Name, LocalVariable))
      @ (ctor.Parameters |> List.map (fun p -> fst p, Argument))
      @ (fields |> List.map (fun f -> (f.Name, Field)))
        |> Map.ofList
    let baseInitialiserArgs =
        ctor.BaseClassConstructorCall 
        |> List.collect (convertExpression identifiers Static)
    { 
        Parameters = ctor.Parameters 
                     |> List.map (fun p -> { Name = fst p; Type = Identifier.typeId (snd p) })
        Body = [LdargIdx 0s]
               @ baseInitialiserArgs
               @ [CallConstructor(baseType, baseArgTypes)]
               @ convertStatements Static identifiers (CompositeStatement ctor.Body)
               @ (fieldInitializers identifiers fields)
        LocalVariables = findLocalVariables ctor.Body
    }
let buildDefaultConstructor (fields : Field<InferredTypeExpression> list) baseType =
    let identifiers = 
        (fields |> List.map (fun f -> (f.Name, Field)))
        |> Map.ofList
    let instructions = [LdargIdx 0s; CallConstructor(baseType, [])] @ fieldInitializers identifiers fields
    { 
        Parameters = []
        Body = instructions @ if noRetInstruction instructions then [Ret] else []
        LocalVariables = []
    }

let private buildClass (c : Ast.ModuleClass<InferredTypeExpression>) : IR.Class = 
    let baseType = c.BaseClass |> Option.map Identifier.typeId |> Option.defaultValue Identifier.object
    let fieldNames = (c.Fields |> List.map(fun f -> f.Name))
    {
        Identifier = c.Identifier
        Methods = c.Functions |> List.map (buildFunction Instance fieldNames)
        Fields = c.Fields |> List.map buildField
        BaseClass = baseType
        Constructors = 
            match c.Constructors with
            | [] -> [buildDefaultConstructor c.Fields baseType]
            | ctors -> ctors |> List.map (buildConstructor c.Fields baseType )
    }

let private buildModule (modul : Module<InferredTypeExpression>) : IR.Module = {
    Identifier = modul.Identifier
    Classes = modul.Classes |> List.map buildClass
    Functions = modul.Functions |> List.map (buildFunction Static [])
} 
let generateIR modules : IR.Module list =
    modules |> List.map buildModule
