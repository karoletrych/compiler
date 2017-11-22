module Compiler.CILGeneration
open System.Reflection.Emit
open System.Reflection
open IR
open Ast

type TypeLookupTable = {
    TypeBuilders : Map<TypeIdentifier, TypeBuilder>
    ExternalTypes : Map<TypeIdentifier, System.Type>
}
with member this.FindType id : System.Type = 
        this.TypeBuilders 
        |> Map.tryFind id 
        |> function 
           | Some t -> t :> System.Type 
           | None -> (this.ExternalTypes |> Map.find id)
     member this.FindMethod tId (methodName : string, argTypes : TypeIdentifier list) = 
            this.FindType tId
            |> (fun t -> t.GetMethods(BindingFlags.Static ||| BindingFlags.Public))
            |> Array.find(fun m ->
                          m.Name = methodName 
                            && m.GetParameters() 
                            |> List.ofArray
                            |> List.map (fun p -> 
                                                  p.GetType()
                                                  |> Identifier.fromDotNet) = argTypes)
       
let generateMethodBody 
    (typesTable : TypeLookupTable) 
    (methodBuilder : MethodBuilder) 
    (instructions : ILInstruction list) = 
    let emitInstruction (ilGenerator : ILGenerator) = 
        function
        | Add        -> ilGenerator.Emit(OpCodes.Add)
        | Call(t, name, argTypes) -> 
            let methodInfo = typesTable.FindMethod t (name, argTypes)
            ilGenerator.Emit(OpCodes.Call, methodInfo)
        | Ceq        -> ilGenerator.Emit(OpCodes.Ceq)
        | Cge        -> ilGenerator.Emit(OpCodes.Clt)
                        ilGenerator.Emit(OpCodes.Ldc_I4_0)
                        ilGenerator.Emit(OpCodes.Ceq)
        | Cgt        -> ilGenerator.Emit(OpCodes.Cgt)
        | Cle        -> ilGenerator.Emit(OpCodes.Cgt)
                        ilGenerator.Emit(OpCodes.Ldc_I4_0)
                        ilGenerator.Emit(OpCodes.Ceq)
        | Clt        -> ilGenerator.Emit(OpCodes.Clt)
        | Dup        -> ilGenerator.Emit(OpCodes.Dup)
        | Div        -> ilGenerator.Emit(OpCodes.Div)
        | Ldarg(i)   -> ilGenerator.Emit(OpCodes.Ldarg, i)
        | Ldc_I4(i)  -> ilGenerator.Emit(OpCodes.Ldc_I4, i)
        | Ldc_R8(r)  -> ilGenerator.Emit(OpCodes.Ldc_R8, r)
        | Ldelem(t)  -> ilGenerator.Emit(OpCodes.Ldelem, (t.ToString()))
        | Ldlen      -> ilGenerator.Emit(OpCodes.Ldlen)
        | Ldloc(i)   -> ilGenerator.Emit(OpCodes.Ldloc, i)
        | Mul        -> ilGenerator.Emit(OpCodes.Mul)
        | Neg        -> ilGenerator.Emit(OpCodes.Neg)
        | Newarr(t)  -> ilGenerator.Emit(OpCodes.Newarr, (t.ToString()))
        | Pop        -> ilGenerator.Emit(OpCodes.Pop)
        | Rem        -> ilGenerator.Emit(OpCodes.Rem)
        | Ret        -> ilGenerator.Emit(OpCodes.Ret)
        | Starg(i)   -> ilGenerator.Emit(OpCodes.Starg, i)
        | Stelem(t)  -> ilGenerator.Emit(OpCodes.Stelem, (t.ToString()))
        | Stloc(i)   -> ilGenerator.Emit(OpCodes.Stloc, i)
        | Sub        -> ilGenerator.Emit(OpCodes.Sub)
    let ilGenerator = methodBuilder.GetILGenerator()
    let instructions = 
        instructions @ 
            if not (instructions 
                    |> List.exists (function
                                     | Ret -> true
                                     | _ -> false))
            then [Ret]
            else []
    instructions 
    |> List.iter (emitInstruction ilGenerator)
    


let private defineStaticMethod 
    (types : TypeLookupTable) 
    (typeBuilder : TypeBuilder) 
    (method : IR.Method) = 
        typeBuilder.DefineMethod(
            method.Name,
            MethodAttributes.Public 
                ||| MethodAttributes.Static 
                ||| MethodAttributes.HideBySig,
            CallingConventions.Standard,
            types.FindType(method.ReturnType),
            method.Parameters 
                |> List.map (fun p -> types.FindType(p.Type))
                |> List.toArray
            );

let private generateModule
    (types : TypeLookupTable) 
    (typeBuilders : TypeBuilder list)
    (modul : IR.Module) =

    let typeBuilder = 
        typeBuilders 
        |> List.find (fun tb -> tb.FullName = (modul.Identifier.ToString()))
    for f in modul.Functions do
        let methodBuilder = defineStaticMethod types typeBuilder f
        generateMethodBody types methodBuilder f.Body
    typeBuilder.CreateType()

let generateAssembly 
    (assemblyBuilder : AssemblyBuilder) 
    (referencedAssemblies : Assembly list)
    (modules : IR.Module list) 
    (setEntryPoint : bool) =
    let moduleBuilder = 
        assemblyBuilder.DefineDynamicModule(
            "Class1.mod",
            assemblyBuilder.GetName().Name, false)

    let externalTypes : Map<TypeIdentifier, System.Type> =
        referencedAssemblies 
        |> List.collect (fun a -> a.GetExportedTypes() |> List.ofArray)
        |> List.map (fun t -> (Identifier.fromDotNet t, t))
        |> Map.ofList

    let typeBuilders =
        modules 
        |> List.map (fun m ->
                m.Identifier,
                moduleBuilder.DefineType(m.Identifier.ToString()))
        |> List.append
                (modules
                |> List.collect (fun m ->
                    m.Classes 
                        |> List.map (fun m ->
                            m.Identifier, moduleBuilder.DefineType((m.Identifier.ToString())))
                        ))

    let typesLookupTable = {
        TypeBuilders = typeBuilders |> Map.ofList
        ExternalTypes = externalTypes
    }
    let types = 
        modules 
        |> List.map (
                generateModule 
                    typesLookupTable 
                    (typeBuilders |> List.map snd))
    if setEntryPoint
    then types 
        |> List.collect (fun t -> t.GetMethods(BindingFlags.Static ||| BindingFlags.Public) |> List.ofArray)
        |> List.filter (fun m -> m.Name = "main")
        |> List.exactlyOne
        |> assemblyBuilder.SetEntryPoint
