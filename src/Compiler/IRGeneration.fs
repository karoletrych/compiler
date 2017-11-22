module Compiler.IRGeneration
open IR
open Ast
open TypeInference

let private convertIdentifier i =
        // match lookupILVariableScope identifierRef with
        // | FieldScope(v)    -> [ Ldsfld v ]
        // | ArgumentScope(i) -> [ Ldarg i ]
        // | LocalScope(i) -> [ Ldloc i ]
        [Ldloc i]

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
    | BinaryExpression(_, _, _) -> failwith "Not Implemented"
    | FunctionCallExpression(_) -> failwith "Not Implemented"
    | IdentifierExpression(_) -> failwith "Not Implemented"
    | ListInitializerExpression(_) -> failwith "Not Implemented"
    | MemberExpression(_) -> failwith "Not Implemented"
    | NewExpression(_, _) -> failwith "Not Implemented"
    | StaticMemberExpression(t, call) -> 
        let getType expr = 
            let (InferredTypeExpression(expr, t)) = expr
            t
        let args = call.Arguments |> List.collect convertExpression
        args 
        @ [Call(Identifier.typeId t,
                call.Name,
                call.Arguments 
                    |> List.map getType)]
    | UnaryExpression(_, _) -> failwith "Not Implemented"

let rec private buildFunctionBody statements : ILInstruction list =
    let generateIL (s : Statement<InferredTypeExpression>) =
        match s with
        | StaticFunctionCallStatement (t, method) -> 
            let typeId = (Identifier.typeId t)
            (method.Arguments |> List.collect convertExpression)
            @ 
            [Call(typeId, method.Name, [])]
    match statements with
    | head::tail -> generateIL head @ buildFunctionBody tail
    | [] -> []

let private locals statements =
    []

let private buildFunction (func : Function<InferredTypeExpression>) = {
    Name = func.Name
    Body = buildFunctionBody func.Body
    ReturnType = Identifier.typeId func.ReturnType.Value
    Parameters = func.Parameters |> List.map (fun p -> {Name = fst p; Type = Identifier.typeId (snd p) })
    LocalVariables = locals func.Body
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
