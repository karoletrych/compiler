module Compiler.CILGeneration
open System.Reflection.Emit
open System.Reflection
open IR
open Ast
open System

type TypeBuilderWrapper = {
    MethodBuilders : Map<IR.MethodRef, MethodBuilder>
    TypeBuilder : TypeBuilder
}

type TypeTable = {
    TypeBuilders : Map<TypeIdentifier, TypeBuilder>
    ExternalTypes : Map<TypeIdentifier, System.Type>
}
with member this.FindType id : System.Type = 
        this.TypeBuilders 
        |> Map.tryFind id 
        |> function 
           | Some t -> t :> System.Type 
           | None -> (this.ExternalTypes |> Map.find id)
 
type FilledTypeTable = {
    FilledTypeBuilders : Map<TypeIdentifier, TypeBuilderWrapper>
    ExternalTypes : Map<TypeIdentifier, System.Type>
}
with member this.FindType id : System.Type = 
        this.FilledTypeBuilders 
        |> Map.tryFind id 
        |> function 
           | Some t -> t.TypeBuilder :> System.Type 
           | None -> (this.ExternalTypes |> Map.find id)
     member this.FindMethod tId (methodRef : MethodRef) = 
        this.FilledTypeBuilders 
        |> Map.tryFind tId 
        |> function 
        | Some t -> 
            t.MethodBuilders.[methodRef] :> MethodInfo
        | None -> 
            (this.ExternalTypes.[tId]
                |> (fun t -> t.GetMethod(methodRef.Name,
                                 BindingFlags.Static ||| BindingFlags.Public,
                                 null,
                                 methodRef.Parameters 
                                    |> List.map(this.FindType) |> List.toArray,
                                 null)
                                 ))
                    
       
let fillMethodBody 
    (typesTable : FilledTypeTable) 
    (methodBuilder : MethodBuilder) 
    (instructions : ILInstruction list) = 
    let emitInstruction (ilGenerator : ILGenerator) = 
        function
        | Add        -> ilGenerator.Emit(OpCodes.Add)
        | Call(t, name, argTypes) -> 
            let methodInfo = typesTable.FindMethod t {Name = name; Parameters = argTypes}
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
        | Br(_) -> failwith "Not Implemented"
        | Brfalse(_) -> failwith "Not Implemented"
        | Brtrue(_) -> failwith "Not Implemented"
        | Label(_) -> failwith "Not Implemented"
        | Ldc_R4(f) -> ilGenerator.Emit(OpCodes.Ldc_R4, f)
        | Ldstr(s) -> ilGenerator.Emit(OpCodes.Ldstr, s)
        | Ldsfld(_) -> failwith "Not Implemented"
        | Stsfld(_) -> failwith "Not Implemented"
    let ilGenerator = methodBuilder.GetILGenerator()
    
    instructions 
    |> List.iter (emitInstruction ilGenerator)
    
let private defineStaticMethod 
    (types : TypeTable) 
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
            )

let private fillTypesInTable
    (types : TypeTable) 
    (typeBuilder : TypeBuilder)
    (modul : IR.Module) =
    modul.Functions 
    |> List.map 
        (fun f -> {
                    Name = f.Name;
                    Parameters = f.Parameters |> List.map (fun p -> p.Type)
                  },
                   defineStaticMethod types typeBuilder f)
    |> Map.ofList

let generateAssembly 
    (assemblyBuilder : AssemblyBuilder) 
    (referencedAssemblies : Assembly list)
    (modules : IR.Module list) 
    (setEntryPoint : bool) =
    let moduleBuilder = 
        let extension = 
            match setEntryPoint with
            | true -> ".exe"
            | false -> ".dll"
        assemblyBuilder.DefineDynamicModule(
            assemblyBuilder.GetName().Name + ".mod",
            assemblyBuilder.GetName().Name + extension, false)

    let externalTypes : Map<TypeIdentifier, System.Type> =
        referencedAssemblies 
        |> List.collect (fun a -> a.GetExportedTypes() |> List.ofArray)
        |> List.map (fun t -> (Identifier.fromDotNet t, t))
        |> Map.ofList

    let typeBuilders =
        modules 
        |> List.map (fun m ->
                m.Identifier,
                moduleBuilder.DefineType(m.Identifier.ToString())
                )
        |> List.append
                (modules
                |> List.collect (fun m ->
                    m.Classes 
                    |> List.map (fun c ->
                        c.Identifier, moduleBuilder.DefineType(c.Identifier.ToString()))
                        ))

    let typesLookupTable = {
        TypeBuilders = typeBuilders |> Map.ofList
        ExternalTypes = externalTypes
    }
    let filledTypeBuilders = 
        modules 
        |> List.map (fun m -> 
                    let typeBuilder = 
                        typeBuilders 
                        |> List.map snd
                        |> List.find (fun tb -> tb.FullName = m.Identifier.ToString())
                    let methodBuilders = fillTypesInTable typesLookupTable typeBuilder m
                    m.Identifier,
                    {
                        MethodBuilders = methodBuilders
                        TypeBuilder = typeBuilder
                    }
                    )
    let filledTypeBuildersMap = filledTypeBuilders |> Map.ofList
    let filledTypesTable = {
            FilledTypeBuilders = filledTypeBuildersMap
            ExternalTypes = externalTypes
        }

    modules 
        |> List.iter (fun m ->
                    m.Functions
                    |> List.iter (fun f ->  
                                    let methodRef = 
                                        {
                                            Name = f.Name; 
                                            Parameters = f.Parameters |> List.map (fun p -> p.Type)
                                        }
                                    let mb = filledTypeBuildersMap
                                                .[m.Identifier]
                                                .MethodBuilders
                                                .[methodRef]
                                    fillMethodBody filledTypesTable mb f.Body)
                        )

    let completeTypes = 
        filledTypeBuilders
        |> List.map (snd  >> (fun tb -> tb.TypeBuilder.CreateType()))

    if setEntryPoint
    then completeTypes 
        |> List.collect (fun t -> 
                            t.GetMethods(BindingFlags.Static ||| BindingFlags.Public) 
                            |> List.ofArray)
        |> List.filter (fun m -> m.Name = "main")
        |> List.exactlyOne
        |> assemblyBuilder.SetEntryPoint


    // typeBuilder.CreateType()
    // generateMethodBody types (methodBuilders.[f]) f.Body

