module Compiler.IRGeneration
open IR
open Ast
open TypeInference
open Compiler.AstProcessing
let private convertIdentifier i =
        // match lookupILVariableScope identifierRef with
        // | FieldScope(v)    -> [ Ldsfld v ]
        // | ArgumentScope(i) -> [ Ldarg i ]
        // | LocalScope(i) -> [ Ldloc i ]
        [Ldloc i]

type GeneratorState = {
    LocalVariableIndex : uint16
}
let getType expr = 
    let (InferredTypeExpression(expr, t)) = expr
    t
let rec private convertExpression (expr : InferredTypeExpression) =
    let (InferredTypeExpression(expr, t)) = expr
    match expr with
    | LiteralExpression l -> 
        match l with 
        | BoolLiteral b -> [ (if b then Ldc_I4(1) else Ldc_I4(0)) ]
        | IntLiteral i -> [ Ldc_I4(i) ]
        | FloatLiteral f -> [ Ldc_R4(f) ]
        | StringLiteral s -> [Ldstr(s)]
    | AssignmentExpression(_, _) -> failwith "Not Implemented"
    | BinaryExpression(e1, op, e2) ->
        convertExpression e1 
        @ convertExpression e2
        @
        match op with
        | ConditionalOr -> failwith "TODO:"
        | ConditionalAnd ->failwith  "TODO:"
        | Equal ->failwith  "TODO:"
        | NotEqual ->failwith  "TODO:"
        | LessEqual ->failwith  "TODO:"
        | Less ->failwith  "TODO:"
        | GreaterEqual ->failwith  "TODO:"
        | Greater ->failwith  "TODO:"
        | Plus -> [Add]
        | Minus -> failwith  "TODO:"
        | Multiplication-> failwith  "TODO:"
        | Division -> failwith  "TODO:"
        | Remainder -> failwith  "TODO:"
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
                            IsStatic = false
                        })
                        ]
    | IdentifierExpression(i) -> [Identifier(i)]
    | ListInitializerExpression(_) -> failwith "Not Implemented"
    | InstanceMemberExpression(_) -> failwith "Not Implemented"
    | NewExpression(_, _) -> failwith "Not Implemented"
    | StaticMemberExpression(t, m) -> 
        match m with
        | MemberFunctionCall call ->
            let args = call.Arguments |> List.collect convertExpression
            args 
            @ [CallMethod(Identifier.typeId t, { MethodName = call.Name; Parameters = call.Arguments |> List.map getType; IsStatic = true})]
        | MemberField f ->
              [GetField(Identifier.typeId t, { FieldName = f; IsStatic = true } )]
    | UnaryExpression(_, _) -> failwith "Not Implemented"
    | LocalFunctionCallExpression(_) -> failwith "Not Implemented"

let rec private buildFunctionBody statements : ILInstruction list =
    let generateIR (s : Statement<InferredTypeExpression>) =
        let getType expr = 
            let (InferredTypeExpression(expr, t)) = expr
            t
        match s with
        | StaticFunctionCallStatement (t, method) -> 
            let typeId = (Identifier.typeId t)
            (method.Arguments |> List.collect convertExpression)
            @ 
            [CallMethod(typeId, {MethodName = method.Name; Parameters = method.Arguments |> List.map getType; IsStatic = true})]
        | AssignmentStatement(_, _) -> failwith "Not Implemented"
        | BreakStatement -> failwith "Not Implemented"
        | CompositeStatement(_) -> failwith "Not Implemented"
        | FunctionCallStatement(_) -> failwith "Not Implemented"
        | IfStatement(_, _, _) -> failwith "Not Implemented"
        | InstanceMemberFunctionCallStatement(_, _) -> failwith "Not Implemented"
        | ReturnStatement(_) -> failwith "Not Implemented"
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
            @
            [DeclareLocal(name, 
                match t with 
                | Some t -> Identifier.fromTypeSpec t
                | None -> init |> getType);
                Stloc(name)]
        | WhileStatement(_, _) -> failwith "Not Implemented"
    let instructions = 
        statements 
        |> List.collect generateIR
    instructions @ 
    if not (instructions 
            |> List.exists (function
                             | Ret -> true
                             | _ -> false))
    then [Ret]
    else []

let private buildFunction (func : Function<InferredTypeExpression>) : IR.Method = {
    Name = func.Name
    Body = buildFunctionBody func.Body
    ReturnType = Identifier.typeId func.ReturnType.Value
    Parameters = func.Parameters |> List.map (fun p -> {Name = fst p; Type = Identifier.typeId (snd p) })
}

let private buildProperty (prop : Ast.Property<InferredTypeExpression>) = {
    Name = prop.Name;
    Type = prop.Type |> Identifier.typeId
}

let private buildClass (c : Ast.ModuleClass<InferredTypeExpression>) : IR.Class = {
    Identifier = c.Identifier
    Methods = c.Functions |> List.map buildFunction
    Properties = c.Properties |> List.map buildProperty
}

let private buildModule (modul : Module<InferredTypeExpression>) : IR.Module = {
        Identifier = modul.Identifier
        Classes = modul.Classes |> List.map buildClass
        Functions = modul.Functions |> List.map buildFunction
    } 
let generateIR modules : IR.Module list =
    modules |> List.map buildModule
