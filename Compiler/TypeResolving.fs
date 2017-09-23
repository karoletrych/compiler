#if !(INTERACTIVE)
module Compiler.TypeResolving
#endif

#if INTERACTIVE
#load @"Ast.fs"
#load @"Types.fs"
#endif

open Compiler.Types
open Compiler.Ast
open System
open System.Reflection

module ExternalTypes = 
        let rec createTypeFromDotNetType (dotnetType : System.Type) : Compiler.Types.Type = 
                let uniqueName (t : Type) = t.ToString()
                let createParameter (dotnetParameter : ParameterInfo) = 
                        {
                                Type = uniqueName dotnetParameter.ParameterType;
                                ParameterName = dotnetParameter.Name
                        }
                let createMethod (dotnetMethod : MethodInfo) =
                        {
                                Parameters = dotnetMethod.GetParameters() |> Array.map createParameter;
                                ReturnType = uniqueName dotnetMethod.ReturnType;
                                MethodName = dotnetMethod.Name
                        }
                let createConstructor (dotnetConstructor : ConstructorInfo) = 
                        {
                        Parameters = dotnetConstructor.GetParameters() |> Array.map createParameter;
                        }
                let createField (dotnetField : Reflection.FieldInfo) =
                        dotnetField.Name, uniqueName dotnetField.FieldType
                {
                    AssemblyName = dotnetType.AssemblyQualifiedName;
                    BaseType = if dotnetType.BaseType <> null then 
                                   Some (uniqueName dotnetType.BaseType)
                               else None;
                    DeclaredConstructors = dotnetType.GetConstructors() |> Array.map createConstructor;
                    Fields = dotnetType.GetFields() |> Array.map createField;
                    Guid = dotnetType.GUID;
                    Name = dotnetType.ToString();
                    GenericParameters = 
                                    if dotnetType.IsGenericParameter then 
                                        dotnetType.GetGenericParameterConstraints() 
                                        |> Array.map uniqueName 
                                    else [||]
                    ImplementedInterfaces = dotnetType.GetInterfaces() |> (Array.map uniqueName);
                    Methods = dotnetType.GetMethods() |> Array.map createMethod
                }

        let mscorlibTypes =
                let mscorlib = Assembly.GetAssembly(typeof<obj>)
                mscorlib.GetTypes()
                |> List.ofArray
                |> List.map (createTypeFromDotNetType >> (fun t -> (t.Name, t)))
                |> Map.ofList

module TypesScanner = 
        type TypeSpecScanResult =
        | ValidTypeSpec
        | InvalidTypeSpec of TypeSpec

        let scanAst (scanType : TypeSpec -> TypeSpecScanResult) : Declaration list -> TypeSpecScanResult list list =
                let get = List.choose id 
                let rec scanStatement (statement : Statement) : TypeSpecScanResult list = 
                        match statement with
                        | FunctionCallStatement(FunctionCall(id, types, args))
                                -> types |> List.map scanType
                        | StaticFunctionCallStatement(t, FunctionCall(id,types,args))
                                -> scanType t :: (types |> List.map scanType)
                        | ValueDeclaration(id, typeSpec, expr)
                                -> get [typeSpec] |> List.map scanType
                        | VariableDeclaration(vd)
                                -> match vd with
                                   | DeclarationWithType (i,t) -> [scanType t]
                                   | FullDeclaration (i,t,e) -> [scanType t]
                                   | DeclarationWithInitialization _ -> []
                        | CompoundStatement(cs)
                                -> cs |> List.collect scanStatement

                let rec scanDeclaration = function
                | FunctionDeclaration(
                                        identifier,
                                        genericParameters,
                                        parameters,
                                        returnType,
                                        cs)
                  -> ((get [returnType]) 
                        @ (parameters |> List.map snd)
                  |> List.map scanType)
                     @ (cs |> List.collect scanStatement)
                | ClassDeclaration({
                                      Type = name;
                                      GenericTypeParameters = typeParameters;
                                      BaseTypes = baseTypes;
                                      ValueDeclarations = values;
                                      FieldDeclarations = variables;
                                      Constructor = constructor;
                                      FunctionDeclarations = methods})
                  ->
                   let scanConstructor : TypeSpecScanResult list = 
                           match constructor with
                           | None -> []
                           | Some c ->
                             let { Parameters = parameters; BaseClassConstructorCall = baseCall; Statements = stmts} = c
                             (parameters |> List.map (snd >> scanType)) @ (stmts |> List.collect scanStatement)

                   (baseTypes |> List.map (CustomTypeSpec >> scanType))
                        @ (values |> List.collect (fun (i,t,e) -> get [t] |> List.map scanType))
                        @ (variables |> List.collect
                                        (function
                                         | DeclarationWithType (i,t) -> [scanType t]
                                         | FullDeclaration (i,t,e) -> [scanType t]
                                         | DeclarationWithInitialization _ -> []))
                        @ scanConstructor
                List.map scanDeclaration

        let scanType 
                (typesDictionary : Map<string, Compiler.Types.Type>)
                (typeSpec : TypeSpec) =
                match typeSpec with
                | CustomTypeSpec (ns, CustomType(id, []))  ->
                        if typesDictionary.ContainsKey (typeSpec.ToString()) 
                        then ValidTypeSpec
                        else InvalidTypeSpec(typeSpec)
                | CustomTypeSpec (ns, CustomType(id, generics))  ->
                        let noGenericsTypeName = (CustomTypeSpec (ns, (CustomType(id, [])))).ToString()
                        let typeWithNumberOfGenerics = (noGenericsTypeName + "`" + (List.length generics).ToString() )
                        match typesDictionary
                                 |> Map.tryPick (fun key v -> if key.StartsWith(typeWithNumberOfGenerics) then Some v else None ) with
                        | Some v ->
                                if (generics |> List.map ((fun g -> g.ToString()) >> typesDictionary.ContainsKey)) |> List.forall ((=) true)
                                then ValidTypeSpec
                                else InvalidTypeSpec(typeSpec)
                        | None -> InvalidTypeSpec(typeSpec)
                | _ -> ValidTypeSpec


let scanTypes = TypesScanner.scanAst (TypesScanner.scanType ExternalTypes.mscorlibTypes)


        // let getBuiltInType =
        //         function 
        //         | Ast.IntLiteral(_) -> createBasicType "int"
        //         | Ast.FloatLiteral(_) -> createBasicType "float"
        //         | Ast.StringLiteral(_) -> createBasicType "string"
        //         | Ast.BoolLiteral(_) -> createBasicType "bool"

        // ExternalTypes.mscorlibTypes;;