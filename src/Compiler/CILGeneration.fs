module Compiler.CILGeneration
open System.Reflection.Emit
open System.Reflection
open IR
open Ast

let findGenericTypeDefinition (types : Map<TypeIdentifier, System.Type>) id =
   types
   |> Map.pick (fun k v -> if k.Namespace = id.Namespace && k.TypeName.Name = id.TypeName.Name  && k.GenericArgumentsNumber = id.GenericArgumentsNumber then Some v else None)

type TypeBuilderWrapper = {
    MethodBuilders : Map<IR.MethodRef, MethodBuilder>
    FieldBuilders : Map<string, FieldBuilder>
    ConstructorBuilders : Map<TypeIdentifier list, ConstructorBuilder>
    TypeBuilder : TypeBuilder
}

type TypeTable = {
    TypeBuilders : Map<TypeIdentifier, TypeBuilder>
    ExternalTypes : Map<TypeIdentifier, System.Type>
}
with member this.FindType id : System.Type = 
            match id.TypeName.GenericArguments with
            | [] ->
                this.TypeBuilders 
                |> Map.tryFind id 
                |> function 
                   | Some t -> t :> System.Type
                   | None -> (this.ExternalTypes |> Map.find id)
            | generics ->
                let t = findGenericTypeDefinition this.ExternalTypes id
                let genericArgs = generics |> List.map this.FindType
                t.MakeGenericType(genericArgs |> List.toArray)
                
 
type Field = 
| Property of MethodInfo
| Field of FieldInfo

type FilledTypeTable = {
    FilledTypeBuilders : Map<TypeIdentifier, TypeBuilderWrapper>
    ExternalTypes : Map<TypeIdentifier, System.Type>
}
with 
        member this.FindType id : System.Type = 
            match id.TypeName.GenericArguments with
            | [] ->
                this.FilledTypeBuilders 
                |> Map.tryFind id 
                |> function 
                   | Some t -> t.TypeBuilder :> System.Type 
                   | None -> (this.ExternalTypes |> Map.find id)
            | generics ->
                let t = findGenericTypeDefinition this.ExternalTypes id
                let genericArgs = generics |> List.map this.FindType
                t.MakeGenericType(genericArgs |> List.toArray)
        member this.FindMethod tId (methodRef : MethodRef) = 
            let bindingFlags =
                match methodRef.Context with
                | Static -> BindingFlags.Static ||| BindingFlags.Public
                | Instance -> BindingFlags.Instance ||| BindingFlags.Public
            let t = this.FindType tId
            t
            |> function
                | :? TypeBuilder ->
                    let tb = this.FilledTypeBuilders.[tId]
                    let methodBuilder = tb.MethodBuilders |> Map.tryFind methodRef |> Option.map (fun m -> m :> MethodInfo)
                    match methodBuilder with
                    | Some m -> m
                    | None -> 
                        let baseClassMethod = this.FindMethod (Identifier.fromDotNet tb.TypeBuilder.BaseType) methodRef
                        baseClassMethod
                | externalT ->
                    if tId.GenericArgumentsNumber <> 0 
                    then
                        let t = findGenericTypeDefinition this.ExternalTypes tId 
                        let methodRef = t.GetMethod(methodRef.MethodName)
                        //TODO: ^^^ only name?
                        TypeBuilder.GetMethod(externalT, methodRef)
                    else
                        externalT.GetMethod(methodRef.MethodName,
                                             bindingFlags,
                                             null,
                                             methodRef.Parameters 
                                                |> List.map(this.FindType) |> List.toArray,
                                             null)
        member this.FindConstructor (tId : TypeIdentifier) (argTypes : TypeIdentifier list) : ConstructorInfo=
            let t = this.FindType tId
            t
            |> function
            | :? TypeBuilder ->
                let tb = this.FilledTypeBuilders.[tId]
                let constructorBuilder = 
                    tb.ConstructorBuilders 
                    |> Map.find argTypes
                constructorBuilder :> ConstructorInfo
            | externalT ->
                if tId.GenericArgumentsNumber <> 0 
                    then
                        let t = findGenericTypeDefinition this.ExternalTypes tId 
                        let constructorRef = t.GetConstructor(argTypes |> List.map this.FindType |> List.toArray)
                        TypeBuilder.GetConstructor(externalT, constructorRef)
                    else
                        externalT.GetConstructor(argTypes |> List.map this.FindType |> List.toArray)

        member this.FindField tId fieldRef = 
            let bindingFlags =
                    match fieldRef.IsStatic with
                    | true -> BindingFlags.Static ||| BindingFlags.Public
                    | false -> BindingFlags.Instance ||| BindingFlags.Public
                
            let t = this.FindType tId
            t
            |> function
                | :? TypeBuilder ->
                    let tb = this.FilledTypeBuilders.[tId]
                    let fieldBuilder = tb.FieldBuilders |> Map.tryFind fieldRef.FieldName |> Option.map (fun m -> m :> FieldInfo)
                    match fieldBuilder with
                    | Some f -> Field f
                    | None -> 
                        let baseClassMethod = this.FindField (Identifier.fromDotNet tb.TypeBuilder.BaseType) fieldRef
                        baseClassMethod
                | externalT ->
                    if tId.GenericArgumentsNumber <> 0 
                    then
                        let unconstructedT = findGenericTypeDefinition this.ExternalTypes tId 
                        let dotnetField = unconstructedT.GetField(fieldRef.FieldName)
                        match dotnetField with
                        | null -> 
                            let tProperty = unconstructedT.GetProperty(fieldRef.FieldName, bindingFlags).GetMethod
                            Property (TypeBuilder.GetMethod(externalT, tProperty))
                        | f -> Field (TypeBuilder.GetField(externalT, f))
                    else
                        match externalT.GetField(fieldRef.FieldName) with
                        | null -> Property (externalT.GetProperty(fieldRef.FieldName, bindingFlags).GetMethod)
                        | f -> Field f
                    
type MethodBuilderState = {
    Labels : Map<int, Label>
}
let private initialState = {Labels = Map.empty}

let findField (typesTable : FilledTypeTable) t fieldRef = 
     match typesTable.FindField t fieldRef with
     | Property _ -> failwith "only fields are supported by InferLang"
     | Field f -> f

let emitInstruction 
    (typesTable : FilledTypeTable) 
    (il : ILGenerator) 
    (returnType : TypeIdentifier) 
    (t : TypeIdentifier) 
    (parameters : IR.Variable list) 
    (variables : Map<string, LocalBuilder>)
    (context : Context)
    (acc : MethodBuilderState) =
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
            if typeInfo.IsValueType then
                let loc = il.DeclareLocal(typeInfo)
                il.Emit(OpCodes.Stloc, loc);
                il.Emit(OpCodes.Ldloca, loc);
                // https://msdn.microsoft.com/en-us/library/system.reflection.emit.opcodes.constrained(v=vs.110).aspx
                il.Emit(OpCodes.Constrained, typeInfo);

            il.Emit(OpCodes.Callvirt, methodInfo);

    function
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
    | Add -> il.Emit(OpCodes.Add)
    | CallMethod(t, methodRef, callee, args) -> 
        callMethod t methodRef
    | CallLocalMethod(methodRef, callee, args) -> 
        callMethod t methodRef
    | GetExternalField (t, fieldRef) ->
        let field = typesTable.FindField t fieldRef
        match field with
        | Property p -> il.Emit(OpCodes.Call, p)
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
    | LdcI4(i)  -> il.Emit(OpCodes.Ldc_I4, i)
    | LdcR8(r)  -> il.Emit(OpCodes.Ldc_R8, r)
    | LdcR4(f) -> il.Emit(OpCodes.Ldc_R4, f)
    | Ldstr(s) -> il.Emit(OpCodes.Ldstr, s)
    | Ldlen      -> il.Emit(OpCodes.Ldlen)
    | Mul        -> il.Emit(OpCodes.Mul)
    | Sub      -> il.Emit(OpCodes.Sub)
    | Neg        -> il.Emit(OpCodes.Neg)
    | CallConstructor(t, argTypes) -> 
        let constructorInfo = typesTable.FindConstructor t argTypes
        il.Emit(OpCodes.Call, constructorInfo)
    | NewObj(t, argTypes) -> 
        let constructorInfo = typesTable.FindConstructor t argTypes
        il.Emit(OpCodes.Newobj, constructorInfo)
    | Pop        -> il.Emit(OpCodes.Pop)
    | Rem        -> il.Emit(OpCodes.Rem)
    | Ret        -> il.Emit(OpCodes.Ret)
    | RetValue(t)-> 
        let t = typesTable.FindType t
        let returnT = typesTable.FindType returnType
        if t.IsValueType && not returnT.IsValueType then il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
    | Starg(i)   -> il.Emit(OpCodes.Starg, i)
    | Stloc(i)   -> 
        il.Emit(OpCodes.Stloc, variables.[i])
    | Ldloc(local) -> 
        il.Emit(OpCodes.Ldloc, variables.[local])
    | Ldarg argName ->
        let argIndex = parameters |> List.findIndex (fun arg -> arg.Name = argName)
        match context with
        | Static -> il.Emit(OpCodes.Ldarg, argIndex)
        | Instance -> il.Emit(OpCodes.Ldarg, argIndex + 1)
        // TODO: Inherited properties
    | Ldfld(field) -> 
        let field = findField typesTable t {FieldName = field; IsStatic = false}
        il.Emit(OpCodes.Ldfld , field)
    | Stfld(field) -> 
        let field = findField typesTable t {FieldName = field; IsStatic = false}
        il.Emit(OpCodes.Stfld , field)
    | Stsfld(field) -> 
        let field = findField typesTable t {FieldName = field; IsStatic = true}
        il.Emit(OpCodes.Stfld , field)
    | LdargIdx(idx) -> il.Emit(OpCodes.Ldarg, idx)
    | Br(_) -> failwith "covered above"
    | Brfalse(_) -> failwith "covered above"
    | Brtrue(_) -> failwith "covered above"
    | Label(_) -> failwith "covered above"
    acc

let private fillMethodBody 
    (t : TypeIdentifier)
    (typesTable : FilledTypeTable) 
    (methodBuilder : MethodBuilder) 
    (method : IR.Method) =
    let il = methodBuilder.GetILGenerator()
    let variables = 
        method.LocalVariables
        |> List.map (fun v -> (v.Name, il.DeclareLocal(typesTable.FindType v.Type)))
        |> Map.ofList
    method.Body
    |> List.fold 
        (fun state instr -> 
                emitInstruction 
                    typesTable 
                    il 
                    method.ReturnType 
                    t 
                    method.Parameters 
                    variables
                    method.Context
                    state instr)
        initialState
    
let private defineMethod 
    (types : TypeTable) 
    (typeBuilder : TypeBuilder) 
    (method : IR.Method) 
    (context : Context) = 
        let attr = 
            match context with
            | Static -> MethodAttributes.Static            
            | Instance -> MethodAttributes.Virtual

        typeBuilder.DefineMethod(
            method.Name,
            MethodAttributes.Public
                ||| attr
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
    (functions : IR.Method list) 
    (context : Context) =
    functions
    |> List.map 
        (fun f -> {
                    MethodName = f.Name;
                    Parameters = f.Parameters |> List.map (fun p -> p.Type)
                    Context = context;
                  },
                   defineMethod types typeBuilder f context)
    |> Map.ofList

let fillBaseType (typesLookupTable : TypeTable) (typeBuilder : TypeBuilder) (c : IR.Class) =
    let t = typesLookupTable.FindType(c.BaseClass)
    typeBuilder.SetParent(t)

let createModuleBuilder (assemblyBuilder : AssemblyBuilder) setEntryPoint = 
    let extension = 
        match setEntryPoint with
        | true -> ".exe"
        | false -> ".dll"
    assemblyBuilder.DefineDynamicModule(
        assemblyBuilder.GetName().Name + ".mod",
        assemblyBuilder.GetName().Name + extension, false)

let getExternalTypes = 
    List.collect (fun (a : Assembly) -> a.GetExportedTypes() |> List.ofArray)
    >> List.map (fun t -> (Identifier.fromDotNet t, t))
    >> Map.ofList

let defineClassType (mb : TypeBuilder) (t : IR.Class) : TypeBuilder =
    mb.DefineNestedType(t.Identifier.TypeName.Name |> List.head, TypeAttributes.Class ||| TypeAttributes.NestedPublic)
let defineModuleType (mb : ModuleBuilder) (m : IR.Module) : TypeBuilder =
    mb.DefineType(m.Identifier.ToString(), TypeAttributes.Class ||| TypeAttributes.Public)

let defineTypes (assemblyBuilder : AssemblyBuilder) 
    (referencedAssemblies : Assembly list)
    (modules : IR.Module list) 
    (setEntryPoint : bool)=
    let moduleBuilder = createModuleBuilder assemblyBuilder setEntryPoint
    let externalTypes = getExternalTypes referencedAssemblies
    let defineModuleType = defineModuleType moduleBuilder

    let typeBuilders =
        modules 
        |> List.collect (fun m ->
            let moduleTypeBuilder = defineModuleType m
            (m.Identifier, moduleTypeBuilder) :: (m.Classes |> List.map (fun c -> c.Identifier, defineClassType moduleTypeBuilder c)))
        |> Map.ofList
    {
        TypeBuilders = typeBuilders
        ExternalTypes = externalTypes
    }

let fillTypes (modules : Module list) (typesLookupTable : TypeTable) =
    let filledTypeBuilders = 
        modules 
        |> List.collect (fun m -> 
            let typeBuilder = typesLookupTable.TypeBuilders.[m.Identifier]
            let methodBuilders = fillTypesInTable typesLookupTable typeBuilder m.Functions Static
            [m.Identifier,
                {
                    MethodBuilders = methodBuilders
                    TypeBuilder = typeBuilder
                    FieldBuilders = Map.empty
                    ConstructorBuilders = Map.empty
                }]
            |> List.append 
                (m.Classes 
                |> List.map(fun c ->
                    let typeBuilder = 
                        typesLookupTable.TypeBuilders.[c.Identifier]
                    let methodBuilders = fillTypesInTable typesLookupTable typeBuilder c.Methods Instance
                    fillBaseType typesLookupTable typeBuilder c

                    c.Identifier,
                    {
                        MethodBuilders = methodBuilders
                        TypeBuilder = typeBuilder
                        FieldBuilders = c.Fields 
                                        |> List.map (fun (p : IR.Variable) -> p.Name, typeBuilder.DefineField(p.Name, typesLookupTable.FindType(p.Type), FieldAttributes.Public))
                                        |> Map.ofList
                        ConstructorBuilders = 
                                c.Constructors 
                                |> List.map (fun ctor -> 
                                    let argTypes = ctor.Parameters |> List.map (fun p -> typesLookupTable.FindType p.Type)
                                    ctor.Parameters |> List.map (fun p -> p.Type), typeBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, argTypes |> Array.ofList))
                                |> Map.ofList
                    }
                )
            )
        )
    let filledTypeBuildersMap = filledTypeBuilders |> Map.ofList
    let filledTypesTable = {
            FilledTypeBuilders = filledTypeBuildersMap
            ExternalTypes = typesLookupTable.ExternalTypes
        }

    let fillFunction typeId context (f : IR.Method)  = 
        let methodRef = 
                    {
                        MethodName = f.Name; 
                        Parameters = f.Parameters |> List.map (fun p -> p.Type)
                        Context = context
                    }
        let mb = filledTypeBuildersMap
                    .[typeId]
                    .MethodBuilders
                    .[methodRef]
        fillMethodBody typeId filledTypesTable mb f |> ignore
    let fillConstructor t (c : IR.Constructor) =
        let ctorParams = c.Parameters |> List.map (fun p -> p.Type)
        let cb = 
            filledTypeBuildersMap.[t].ConstructorBuilders.[ctorParams]
        let il = cb.GetILGenerator()
        let variables = 
            c.LocalVariables
            |> List.map (fun v -> (v.Name, il.DeclareLocal(filledTypesTable.FindType v.Type)))
            |> Map.ofList

        c.Body
        |> List.fold 
            (fun state i -> 
                emitInstruction filledTypesTable il Identifier.``void`` t c.Parameters variables Instance state i)
            initialState |> ignore
    modules 
        |> List.iter (fun m ->
            m.Functions
            |> List.iter (fillFunction m.Identifier Static)
            m.Classes
            |> List.iter (fun c ->
                c.Methods
                |> List.iter (fillFunction c.Identifier Instance)
                c.Constructors
                |> List.iter (fillConstructor c.Identifier)
                )
                )
    filledTypeBuildersMap
let buildTypes (modules : Module list) filledTypeBuilders = 
    let moduleIds = modules |> List.map (fun m -> m.Identifier)
    let (moduleTypeBuilders, classTypeBuilders) = 
        filledTypeBuilders
        |> Map.toList
        |> List.partition (fun t ->  moduleIds |> List.contains (fst t))

    (moduleTypeBuilders |> List.map (snd  >> (fun tb -> tb.TypeBuilder.CreateType())))
  @ (classTypeBuilders |> List.map (snd  >> (fun tb -> tb.TypeBuilder.CreateType())))    

let findEntryPoint =
    List.collect (fun (t : System.Type) -> 
                            t.GetMethods(BindingFlags.Static ||| BindingFlags.Public) 
                            |> List.ofArray)
    >> List.filter (fun m -> m.Name = "main")
    >> List.exactlyOne

let generateAssembly 
    (assemblyBuilder : AssemblyBuilder) 
    (referencedAssemblies : Assembly list)
    (modules : IR.Module list) 
    (setEntryPoint : bool) =

    let typesLookupTable = defineTypes assemblyBuilder referencedAssemblies modules setEntryPoint
    let filledTypeBuilders = fillTypes modules typesLookupTable
    let completeTypes = buildTypes modules filledTypeBuilders

    if setEntryPoint
    then completeTypes 
        |> findEntryPoint
        |> assemblyBuilder.SetEntryPoint
