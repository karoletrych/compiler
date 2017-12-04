module Compiler.CILGeneration
open System.Reflection.Emit
open System.Reflection
open IR
open Ast

type TypeBuilderWrapper = {
    MethodBuilders : Map<IR.MethodRef, MethodBuilder>
    PropertyBuilders : Map<string, PropertyBuilder>
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
 
type Field = 
| Property of PropertyInfo
| Field of FieldInfo

type FilledTypeTable = {
    FilledTypeBuilders : Map<TypeIdentifier, TypeBuilderWrapper>
    ExternalTypes : Map<TypeIdentifier, System.Type>
}
with 
        member this.FindType id : System.Type = 
            this.FilledTypeBuilders 
            |> Map.tryFind id 
            |> function 
               | Some t -> t.TypeBuilder :> System.Type 
               | None -> (this.ExternalTypes |> Map.find id)
        member this.FindMethod tId (methodRef : MethodRef) = 
            let bindingFlags =
                match methodRef.Context with
                | Static -> BindingFlags.Static ||| BindingFlags.Public
                | Instance -> BindingFlags.Instance ||| BindingFlags.Public
            this.FilledTypeBuilders 
            |> Map.tryFind tId 
            |> function 
            | Some t -> 
                t.MethodBuilders.[methodRef] :> MethodInfo
            | None -> 
                (this.ExternalTypes.[tId]
                    |> (fun t -> t.GetMethod(methodRef.MethodName,
                                     bindingFlags,
                                     null,
                                     methodRef.Parameters 
                                        |> List.map(this.FindType) |> List.toArray,
                                     null)
                                     ))
        member this.FindField tId propertyRef = 
            let bindingFlags =
                    match propertyRef.IsStatic with
                    | true -> BindingFlags.Static ||| BindingFlags.Public
                    | false -> BindingFlags.Instance ||| BindingFlags.Public
            this.FilledTypeBuilders 
            |> Map.tryFind tId 
            |> function 
            | Some t -> 
                Property (t.PropertyBuilders.[propertyRef.FieldName] :> PropertyInfo)
            | None -> 
                (this.ExternalTypes.[tId]
                    |> (fun t -> 
                        match t.GetProperty(propertyRef.FieldName, bindingFlags) with
                        | null -> Field (t.GetField(propertyRef.FieldName, bindingFlags))
                        | t -> Property t
                        )
                )
                    
type MethodBuilderState = {
    LocalVariables : Map<string, LocalBuilder>
    Labels : Map<int, Label>
}
let private startState = {LocalVariables = Map.empty; Labels = Map.empty}
let private fillMethodBody 
    (t : TypeIdentifier)
    (typesTable : FilledTypeTable) 
    (methodBuilder : MethodBuilder) 
    (method : IR.Method)=

    let emitInstruction (il : ILGenerator) (acc : MethodBuilderState) =
        let useLabel foo (l : int) =
            let result =
                match acc.Labels.TryFind l with
                | Some l ->  l, acc.Labels
                | None -> 
                    let label = il.DefineLabel()
                    label, acc.Labels.Add(l, label)
            foo (fst result)
            {acc with Labels = snd result}
        let callMethod t methodRef = 
            let methodInfo = typesTable.FindMethod t methodRef
            match methodRef.Context with
            | Static -> 
                il.Emit(OpCodes.Call, methodInfo)
            | Instance -> 
                let typeInfo = typesTable.FindType t
                let loc = il.DeclareLocal(typeInfo)
                il.Emit(OpCodes.Stloc, loc);
                il.Emit(OpCodes.Ldloca, loc);
                // https://msdn.microsoft.com/en-us/library/system.reflection.emit.opcodes.constrained(v=vs.110).aspx
                il.Emit(OpCodes.Constrained, typeInfo);
                il.Emit(OpCodes.Callvirt, methodInfo);
 
        function
        | DeclareLocal(name, t) -> 
            let local = il.DeclareLocal(typesTable.FindType t)
            {acc with LocalVariables = acc.LocalVariables.Add(name, local) }
        | Label(l) -> 
            l |> useLabel (fun label -> il.MarkLabel(label))
        | Br(l) -> 
            l |> useLabel (fun label -> il.Emit(OpCodes.Br, label))
        | Brfalse(l) -> 
            l |> useLabel (fun label -> il.Emit(OpCodes.Brfalse, label))
        | Brtrue(l) -> 
            l |> useLabel (fun label -> il.Emit(OpCodes.Brtrue, label))
        | other -> 
        other |> function
        | DeclareLocal _ -> failwith "error"
        | Add -> il.Emit(OpCodes.Add)
        | CallMethod(t, methodRef) -> 
            callMethod t methodRef
        | CallLocalMethod(methodRef) -> 
            callMethod t methodRef
        | GetField (t, fieldRef) ->
            let field = typesTable.FindField t fieldRef
            match field with
            | Property p -> il.Emit(OpCodes.Call, p.GetMethod)
            | Field f -> 
                         match fieldRef.IsStatic with
                         | true -> il.Emit(OpCodes.Ldsfld, f)
                         | false -> il.Emit(OpCodes.Ldfld, f)
        | Ceq        -> il.Emit(OpCodes.Ceq)
        | Cge        -> il.Emit(OpCodes.Clt)
                        il.Emit(OpCodes.Ldc_I4_0)
                        il.Emit(OpCodes.Ceq)
        | Cgt        -> il.Emit(OpCodes.Cgt)
        | Cle        -> il.Emit(OpCodes.Cgt)
                        // TODO: other types than int
                        il.Emit(OpCodes.Ldc_I4_0)
                        il.Emit(OpCodes.Ceq)
        | Clt        -> il.Emit(OpCodes.Clt)
        | Duplicate        -> il.Emit(OpCodes.Dup)
        | Div        -> il.Emit(OpCodes.Div)
        | Ldarg(i)   -> il.Emit(OpCodes.Ldarg, i)
        | Ldc_I4(i)  -> il.Emit(OpCodes.Ldc_I4, i)
        | Ldc_R8(r)  -> il.Emit(OpCodes.Ldc_R8, r)
        | Ldelem(t)  -> il.Emit(OpCodes.Ldelem, (t.ToString()))
        | Ldlen      -> il.Emit(OpCodes.Ldlen)
        | Ldloc(i)   -> il.Emit(OpCodes.Ldloc, i)
        | Mul        -> il.Emit(OpCodes.Mul)
        | Neg        -> il.Emit(OpCodes.Neg)
        | Newarr(t)  -> il.Emit(OpCodes.Newarr, (t.ToString()))
        | Pop        -> il.Emit(OpCodes.Pop)
        | Rem        -> il.Emit(OpCodes.Rem)
        | Ret        -> il.Emit(OpCodes.Ret)
        | RetValue(t)-> 
            let t = typesTable.FindType t
            let returnT = typesTable.FindType method.ReturnType
            if t.IsValueType && not returnT.IsValueType then il.Emit(OpCodes.Box, t)
            il.Emit(OpCodes.Ret)
        | Starg(i)   -> il.Emit(OpCodes.Starg, i)
        | Stelem(t)  -> il.Emit(OpCodes.Stelem, (t.ToString()))
        | Stloc(i)   -> 
            let local = acc.LocalVariables |> Map.find i
            il.Emit(OpCodes.Stloc, local)
        | Sub        -> il.Emit(OpCodes.Sub)
        | Ldc_R4(f) -> il.Emit(OpCodes.Ldc_R4, f)
        | Ldstr(s) -> il.Emit(OpCodes.Ldstr, s)
        | Ldsfld(_) -> failwith "Not Implemented"
        | Stsfld(_) -> failwith "Not Implemented"
        | LoadFromIdentifier(i) -> 
            let local =
                acc.LocalVariables
                |> Map.tryFind i
            match local with
            | Some l -> il.Emit(OpCodes.Ldloc, l);
            | None -> 
                let argIndex = method.Parameters |> List.tryFindIndex (fun arg -> arg.Name = i)
                match argIndex with
                | Some arg -> il.Emit(OpCodes.Ldarg, arg)
                | None ->
                    // TODO: Inherited properties
                    let (Field field) = typesTable.FindField t {FieldName = i; IsStatic = false}
                    il.Emit(OpCodes.Ldfld, field)
        | StoreToIdentifier(assignee) -> 
            match assignee with
            | IdentifierAssignee i -> 
                let local =
                    acc.LocalVariables
                    |> Map.tryFind i
                match local with
                | Some l -> il.Emit(OpCodes.Stloc , l);
                | None -> 
                    let argIndex = method.Parameters |> List.tryFindIndex (fun arg -> arg.Name = i)
                    match argIndex with
                    | Some arg -> il.Emit(OpCodes.Starg, arg)
                    | None -> 
                        // TODO: Inherited properties
                        let (Field field) = typesTable.FindField t {FieldName = i; IsStatic = false}
                        il.Emit(OpCodes.Stsfld , field)
            | MemberFieldAssignee (e, i) -> failwith "not implemented"
        | Br(_) -> failwith "covered above"
        | Brfalse(_) -> failwith "covered above"
        | Brtrue(_) -> failwith "covered above"
        | Label(_) -> failwith "covered above"
        acc

    let ilGenerator = methodBuilder.GetILGenerator()
    method.Body
    |> List.fold 
        (fun state i -> emitInstruction ilGenerator state i)
        startState
    
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
                    MethodName = f.Name;
                    Parameters = f.Parameters |> List.map (fun p -> p.Type)
                    Context = Static;
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
                        PropertyBuilders = Map.empty
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
            |> List.iter (fun (f : Method) ->  
                let methodRef = 
                    {
                        MethodName = f.Name; 
                        Parameters = f.Parameters |> List.map (fun p -> p.Type)
                        Context = Static
                    }
                let mb = filledTypeBuildersMap
                            .[m.Identifier]
                            .MethodBuilders
                            .[methodRef]
                fillMethodBody m.Identifier filledTypesTable mb f |> ignore
                ))
            

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

