module Compiler.IRGeneration
open IR
open Ast
open TypeInference

type GeneratorState = {
    LocalVariableIndex : uint16
}

let rec private convertExpression context (expr : InferredTypeExpression) =
    let convertExpression = convertExpression context
    let (InferredTypeExpression(expr, _)) = expr
    match expr with
    | LiteralExpression l -> 
        match l with 
        | BoolLiteral b -> [ (if b then Ldc_I4(1) else Ldc_I4(0)) ]
        | IntLiteral i -> [ Ldc_I4(i) ]
        | FloatLiteral f -> [ Ldc_R4(f) ]
        | StringLiteral s -> [Ldstr(s)]
    | AssignmentExpression(assignee, expr) -> 
        convertExpression expr
        @ [Duplicate]
        @ [StoreToIdentifier assignee]
    | BinaryExpression(e1, op, e2) ->
        convertExpression e1 
      @ convertExpression e2
      @ match op with
        | ConditionalOr -> failwith "TODO:"
        | ConditionalAnd -> failwith  "TODO:"
        | Equal -> [Ceq]
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
            convertExpression calleeExpression 
          @ [CallMethod(
                calleeT,
                        {
                            MethodName = call.Name
                            Parameters = call.Arguments |> List.map getType
                            Context = Instance
                        })
                        ]
    | IdentifierExpression(i) -> [LoadFromIdentifier(i)]
    | ListInitializerExpression(_) -> failwith "Not Implemented"
    | NewExpression(_, _) -> failwith "Not Implemented"
    | StaticMemberExpression(t, m) -> 
        match m with
        | MemberFunctionCall call ->
            let args = call.Arguments |> List.collect convertExpression
            args 
          @ [CallMethod(Identifier.typeId t, { MethodName = call.Name; Parameters = call.Arguments |> List.map getType; Context = Static})]
        | MemberField f ->
              [GetField(Identifier.typeId t, { FieldName = f; IsStatic = true } )]
    | UnaryExpression(_, _) -> failwith "Not Implemented"
    | LocalFunctionCallExpression(lfc) -> 
        (lfc.Arguments |> List.collect convertExpression)
        @ [CallLocalMethod {
                            MethodName = lfc.Name
                            Parameters = lfc.Arguments |> List.map getType
                            Context = context
                           }]

let random = System.Random()
let randomInt () = random.Next()
let rec private convertStatements isStatic statements : ILInstruction list =
    let convertExpression = convertExpression isStatic
    let rec generateIR (s : Statement<InferredTypeExpression>) =
        match s with
        | StaticFunctionCallStatement (t, method) -> 
            let typeId = (Identifier.typeId t)
            (method.Arguments |> List.collect convertExpression)
            @ 
            [CallMethod(typeId, {MethodName = method.Name; Parameters = method.Arguments |> List.map getType; Context = Static})]
        | AssignmentStatement(assignee, e2) -> 
            convertExpression e2
            @ [StoreToIdentifier assignee]
        | BreakStatement -> failwith "Not Implemented"
        | CompositeStatement(cs) -> cs |> List.collect generateIR
        | FunctionCallStatement(_) -> failwith "Not Implemented"
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

        | InstanceMemberFunctionCallStatement(_, _) -> failwith "Not Implemented"
        | ReturnStatement(r) -> 
            match r with 
            | Some r -> convertExpression r @ [RetValue (getType r)]
            | None -> [Ret]
        | VariableDeclaration(vd) -> 
            match vd with
            | DeclarationWithInitialization (name, init) -> 
                convertExpression init @ [DeclareLocal(name, getType init); Stloc(name)]
            | DeclarationWithType (name, t) -> 
                [DeclareLocal(name, Identifier.typeId t); Stloc(name)] // TODO:
            | FullDeclaration (name, t, init) -> 
                convertExpression init 
                @ [DeclareLocal(name, Identifier.typeId t); Stloc(name)]
        | ValueDeclaration(name, t, init) ->
            convertExpression init 
            @ [DeclareLocal(name, 
                match t with 
                | Some t -> Identifier.fromTypeSpec t
                | None -> init |> getType);
                Stloc(name)]
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
    let noRetInstruction instructions = 
        not (instructions 
           |> List.exists (function
                          | Ret -> true
                          | RetValue _ -> true
                          | _ -> false))
    instructions @ if noRetInstruction instructions then [Ret] else []

let private buildFunction isStatic (func : Function<InferredTypeExpression>) : IR.Method = {
    Name = func.Name
    Body = convertStatements isStatic (CompositeStatement func.Body)
    ReturnType = Identifier.typeId func.ReturnType.Value
    Parameters = 
        func.Parameters 
        |> List.map (fun p -> {Name = fst p; Type = Identifier.typeId (snd p) })
}

let private buildProperty (prop : Ast.Property<InferredTypeExpression>) = {
    Name = prop.Name;
    Type = prop.Type |> Identifier.typeId
}

let private buildClass (c : Ast.ModuleClass<InferredTypeExpression>) : IR.Class = {
    Identifier = c.Identifier
    Methods = c.Functions |> List.map (buildFunction Instance)
    Properties = c.Properties |> List.map buildProperty
}

let private buildModule (modul : Module<InferredTypeExpression>) : IR.Module = {
    Identifier = modul.Identifier
    Classes = modul.Classes |> List.map buildClass
    Functions = modul.Functions |> List.map (buildFunction Static)
} 
let generateIR modules : IR.Module list =
    modules |> List.map buildModule
