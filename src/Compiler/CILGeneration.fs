module Compiler.CILGeneration
open System.Reflection.Emit
open System.Reflection
open IR
open Ast


let findGenericTypeDefinition (types : Map<TypeIdentifier, System.Type>) id =
   types
   |> Map.pick (fun k v -> 
                    if Identifier.equalWithoutGeneric k id
                    then Some v 
                    else None)

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
let rec findType this id : System.Type = 
    match id.GenericParameters with
    | [] ->
        this.TypeBuilders 
        |> Map.tryFind id 
        |> function 
           | Some t -> t :> System.Type
           | None -> (this.ExternalTypes |> Map.find id)
    | generics ->
        let t = findGenericTypeDefinition this.ExternalTypes id
        let genericArgs = generics |> List.map (getGenericArgument >> findType this)
        t.MakeGenericType(genericArgs |> List.toArray)
                
 
type Field = 
| Property of MethodInfo
| Field of FieldInfo

type FilledTypeTable = {
    FilledTypeBuilders : Map<TypeIdentifier, TypeBuilderWrapper>
    ExternalTypes : Map<TypeIdentifier, System.Type>
}
let isBeingBuilt (t : System.Type)  =
    match t with
    | :? TypeBuilder -> true
    | _ -> false

    
let rec findFilledType this id : System.Type = 
    match id.GenericParameters with
    | [] ->
        this.FilledTypeBuilders 
        |> Map.tryFind id 
        |> function 
           | Some t -> t.TypeBuilder :> System.Type 
           | None -> this.ExternalTypes |> Map.find id
    | generics ->
        let unboundType = findGenericTypeDefinition this.ExternalTypes id
        let genericArgs = generics |> List.map (getGenericArgument >> findFilledType this)
        unboundType.MakeGenericType(genericArgs |> List.toArray)
let rec findMethod this tId (methodRef : MethodRef) = 
    let bindingFlags =
        match methodRef.Context with
        | Static -> BindingFlags.Static ||| BindingFlags.Public
        | Instance -> BindingFlags.Instance ||| BindingFlags.Public
    let t = findFilledType this tId
    match isBeingBuilt t with 
    | true ->
        let tb = this.FilledTypeBuilders.[tId]
        let methodBuilder = tb.MethodBuilders |> Map.tryFind methodRef |> Option.map (fun m -> m :> MethodInfo)
        match methodBuilder with
        | Some m -> m
        | None -> 
            let baseClassMethod = findMethod this (Identifier.fromDotNet tb.TypeBuilder.BaseType) methodRef
            baseClassMethod
    | false ->
        if (not (List.isEmpty tId.GenericParameters)) && tId.GenericParameters |> List.exists (fun tId -> findFilledType this (getGenericArgument tId) |> isBeingBuilt)
        then
            let unboundType = findGenericTypeDefinition this.ExternalTypes tId 
            let methodRef = unboundType.GetMethod(methodRef.MethodName)
            //^^^ only by name?
            let method = TypeBuilder.GetMethod(t, methodRef)
            if isNull method then
                failwithf "Method: %A not found" methodRef
            method
        else
            let method = t.GetMethod(methodRef.MethodName,
                                bindingFlags,
                                null,
                                methodRef.Parameters 
                                    |> List.map(findFilledType this) |> List.toArray,
                                null)
            if isNull method then
                failwithf "Method: %A not found" methodRef
            method

let findConstructor this (tId : TypeIdentifier) (argTypes : TypeIdentifier list) : ConstructorInfo =
    let t = findFilledType this tId
    match isBeingBuilt t with
    | true ->
        // constructed type is being defined in InferLang
        let tb = this.FilledTypeBuilders.[tId]
        let constructorBuilder = 
            tb.ConstructorBuilders 
            |> Map.find argTypes
        constructorBuilder :> ConstructorInfo
    | false ->
        if (not (List.isEmpty tId.GenericParameters)) 
            && tId.GenericParameters |> List.exists (fun tId -> findFilledType this (getGenericArgument tId) |> isBeingBuilt)
        // external nongeneric type
        then
            let unboundType = findGenericTypeDefinition this.ExternalTypes tId 
            let constructorRef = unboundType.GetConstructor(argTypes |> List.map (findFilledType this) |> List.toArray)
            TypeBuilder.GetConstructor(t, constructorRef)
        // external generic type
        else
            t.GetConstructor(argTypes |> List.map (findFilledType this) |> List.toArray)

let rec findFieldOrProperty this tId fieldRef = 
    let bindingFlags =
        match fieldRef.IsStatic with
        | true -> BindingFlags.Static ||| BindingFlags.Public
        | false -> BindingFlags.Instance ||| BindingFlags.Public
        
    let t = findFilledType this tId
    match isBeingBuilt t with
    | true ->
        let tb = this.FilledTypeBuilders.[tId]
        let fieldBuilder = tb.FieldBuilders |> Map.tryFind fieldRef.FieldName |> Option.map (fun m -> m :> FieldInfo)
        match fieldBuilder with
        | Some f -> Field f
        | None -> 
            let baseClassMethod = (findFieldOrProperty this) (Identifier.fromDotNet tb.TypeBuilder.BaseType) fieldRef
            baseClassMethod
    | false ->
        if (not (List.isEmpty tId.GenericParameters)) 
                && tId.GenericParameters |> List.exists (fun tId -> findFilledType this (getGenericArgument tId) |> isBeingBuilt)
        then
            let unboundType = findGenericTypeDefinition this.ExternalTypes tId 
            let dotnetField = unboundType.GetField(fieldRef.FieldName)
            match dotnetField with
            | null -> 
                let tProperty = unboundType.GetProperty(fieldRef.FieldName, bindingFlags).GetMethod
                Property (TypeBuilder.GetMethod(t, tProperty))
            | f -> Field (TypeBuilder.GetField(t, f))
        else
            match t.GetField(fieldRef.FieldName) with
            | null -> Property (t.GetProperty(fieldRef.FieldName, bindingFlags).GetMethod)
            | f -> Field f
                    
type MethodBuilderState = {
    Labels : Map<int, Label>
}
type MethodInfo = {
    ReturnType : TypeIdentifier
    OwnerClassType : TypeIdentifier
    Parameters : IR.Variable list
    Variables : Map<string, LocalBuilder>
    Context : Context
}
let private initialState = {Labels = Map.empty}

let findField (typesTable : FilledTypeTable) (t : TypeIdentifier) fieldRef = 
    match findFieldOrProperty typesTable t fieldRef with
    | Property _ -> failwith "only fields are supported by InferLang"
    | Field f -> f

let rec emitInstruction 
    (typesTable : FilledTypeTable) 
    (il : ILGenerator) 
    (acc : MethodBuilderState)
    (methodInfo : MethodInfo) 
    (ilInstruction) : MethodBuilderState =
    let emitInstruction = emitInstruction typesTable il acc methodInfo
    let useLabel foo (l : int) =
        let result =
            match acc.Labels.TryFind l with
            | Some l ->  l, acc.Labels
            | None -> 
                let label = il.DefineLabel()
                label, acc.Labels.Add(l, label)
        foo (fst result)
        {acc with Labels = snd result}
    let generateCallee 
        (variables : Map<string, LocalBuilder>) 
        (il : ILGenerator) 
        (variableTypeInfo : System.Type)
        (calleeInstructions) = 
        let localVariable = 
            let variableName = 
                if calleeInstructions |> List.length <> 1 
                then None
                else
                    match calleeInstructions.[0] with
                    | Ldarg a -> Some a
                    | Ldloc l -> Some l
                    | _ -> None
            variableName |> Option.bind (fun v -> variables |> Map.tryFind v)

        match variableTypeInfo.IsValueType, localVariable with
        | true, Some variableName -> 
            il.Emit(OpCodes.Ldloca, variableName);
        | true, None ->
            calleeInstructions |> List.iter (emitInstruction >> ignore)
            let v = il.DeclareLocal(variableTypeInfo)
            il.Emit(OpCodes.Stloc, v);
            il.Emit(OpCodes.Ldloca, v);
        | _ ->
            calleeInstructions |> List.iter (emitInstruction >> ignore)

    let random = System.Random()
    let randomInt () = random.Next()

    let callMethod t (methodRef : MethodRef) calleeInstructions argsInstructions = 
        let typeInfo = findFilledType typesTable t

        generateCallee methodInfo.Variables il typeInfo calleeInstructions

        argsInstructions |> List.iter (emitInstruction >> ignore)

        let methodInfo = findMethod typesTable t methodRef

        if methodRef.Context = Static || typeInfo.IsValueType
        then
            il.Emit(OpCodes.Call, methodInfo)
        else
            il.Emit(OpCodes.Callvirt, methodInfo);

    match ilInstruction with
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
    | CallMethod(t, methodRef, calleeInstructions, argsInstructions) -> 
        callMethod t methodRef calleeInstructions argsInstructions
    | CallLocalMethod(methodRef, calleeInstructions, argsInstructions) -> 
        callMethod methodInfo.OwnerClassType methodRef calleeInstructions argsInstructions
    
    | Ceq        -> il.Emit(OpCodes.Ceq)
    | Cgt        -> il.Emit(OpCodes.Cgt)
    | Clt        -> il.Emit(OpCodes.Clt)
    | And        -> il.Emit(OpCodes.And)
    | Or         -> il.Emit(OpCodes.Or)
    | Duplicate  -> il.Emit(OpCodes.Dup)
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
        let constructorInfo = findConstructor typesTable t argTypes
        il.Emit(OpCodes.Call, constructorInfo)
    | NewObj(t, argTypes) -> 
        let constructorInfo = findConstructor typesTable t argTypes
        il.Emit(OpCodes.Newobj, constructorInfo)
    | Pop        -> il.Emit(OpCodes.Pop)
    | Rem        -> il.Emit(OpCodes.Rem)
    | Ret        -> il.Emit(OpCodes.Ret)
    | RetValue(t)-> 
        let t = findFilledType typesTable t
        let returnT = findFilledType typesTable methodInfo.ReturnType
        if t.IsValueType && not returnT.IsValueType then il.Emit(OpCodes.Box, t)
        il.Emit(OpCodes.Ret)
    | Starg(i)   -> il.Emit(OpCodes.Starg, i)
    | Stloc(i)   -> 
        il.Emit(OpCodes.Stloc, methodInfo.Variables.[i])
    | Ldloc(local) -> 
        il.Emit(OpCodes.Ldloc, methodInfo.Variables.[local])
    | Ldarg argName ->
        let argIndex = methodInfo.Parameters |> List.findIndex (fun arg -> arg.Name = argName)
        match methodInfo.Context with
        | Static -> il.Emit(OpCodes.Ldarg, argIndex)
        | Instance -> il.Emit(OpCodes.Ldarg, argIndex + 1)
    | Ldfld(field) -> // Field type is not important when looking for it so we supply Identifier.object
        let field = findField typesTable methodInfo.OwnerClassType {FieldName = field; IsStatic = false; FieldType = Identifier.object}
        il.Emit(OpCodes.Ldfld, field)
    | Stfld(field) -> 
        let field = findField typesTable methodInfo.OwnerClassType {FieldName = field; IsStatic = false; FieldType = Identifier.object}
        il.Emit(OpCodes.Stfld, field)
    | Stsfld(field) -> 
        let field = findField typesTable methodInfo.OwnerClassType {FieldName = field; IsStatic = true; FieldType = Identifier.object}
        il.Emit(OpCodes.Stfld , field)
    | LdThis -> il.Emit(OpCodes.Ldarg_0)
    | GetExternalField (t, fieldRef, calleeInstructions) ->
        let field = findFieldOrProperty typesTable t fieldRef
        match field with
        | Property p -> 
            match fieldRef.IsStatic with
            | true -> 
                il.Emit(OpCodes.Call, p)
            | false -> 
                let typeInfo = findFilledType typesTable t
                generateCallee methodInfo.Variables il typeInfo calleeInstructions
                il.Emit(OpCodes.Call, p)
        | Field f -> 
            match fieldRef.IsStatic with
            | true -> 
                il.Emit(OpCodes.Ldsfld, f)
            | false -> 
                let typeInfo = findFilledType typesTable t
                generateCallee methodInfo.Variables il typeInfo calleeInstructions
                il.Emit(OpCodes.Ldfld, f)
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
        |> List.map (fun v -> (v.Name, il.DeclareLocal(findFilledType typesTable v.Type)))
        |> Map.ofList
    method.Body
    |> List.fold 
        (fun state instr -> 
                emitInstruction 
                    typesTable 
                    il 
                    state 
                    {
                        ReturnType = method.ReturnType 
                        OwnerClassType = t 
                        Parameters = method.Parameters 
                        Variables = variables
                        Context = method.Context
                    }
                    instr)
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
            findType types (method.ReturnType),
            method.Parameters 
                |> List.map (fun p -> findType types (p.Type))
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
    let t = findType typesLookupTable (c.BaseClass)
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
    mb.DefineNestedType(t.Identifier.Name, TypeAttributes.Class ||| TypeAttributes.NestedPublic)
let defineModuleType (mb : ModuleBuilder) (m : IR.Module) : TypeBuilder =
    mb.DefineType(m.Identifier.ToString(), TypeAttributes.Class ||| TypeAttributes.Public)

let defineTypes (assemblyBuilder : AssemblyBuilder) 
    (referencedAssemblies : Assembly list)
    (modules : IR.Module list) 
    (setEntryPoint : bool) =
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
                                        |> List.map (fun (p : IR.Variable) -> p.Name, typeBuilder.DefineField(p.Name, findType typesLookupTable (p.Type), FieldAttributes.Public))
                                        |> Map.ofList
                        ConstructorBuilders = 
                                c.Constructors 
                                |> List.map (fun ctor -> 
                                    let argTypes = ctor.Parameters |> List.map (fun p -> findType typesLookupTable p.Type)
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
            |> List.map (fun v -> (v.Name, il.DeclareLocal(findFilledType filledTypesTable v.Type)))
            |> Map.ofList

        c.Body
        |> List.fold 
            (fun state instruction -> 
                emitInstruction 
                    filledTypesTable 
                    il 
                    state
                    {
                        ReturnType = Identifier.``void`` 
                        OwnerClassType = t 
                        Parameters = c.Parameters
                        Variables = variables
                        Context = Instance
                    }
                    instruction)
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
