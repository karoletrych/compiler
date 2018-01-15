module Compiler.ReferencedDllsMetadataRetriever

open System.Reflection
open Ast
open Types
open FSharpx.Collections
open System.Diagnostics
let rec createTypeFromDotNetType (dotnetType : System.Type) : Types.Type = 
    let createGenericParameterInfo (dotnetType : System.Type) = 
        let position = dotnetType.GenericParameterPosition
        let declared = 
            if dotnetType.DeclaringMethod |> isNull
            then
                Class 
            else
                Method 
        (declared, position)
    let createTypeRef (dotnetType : System.Type) : TypeRef= 
        if dotnetType.IsGenericParameter
        then GenericParameter (createGenericParameterInfo dotnetType)
        else
            if dotnetType.ContainsGenericParameters
            then GenericTypeDefinition (Identifier.fromDotNet dotnetType)
            else ConstructedType (Identifier.fromDotNet dotnetType)
    let createParameter (dotnetParameter : ParameterInfo) = 
        {
            Type = createTypeRef dotnetParameter.ParameterType
            ParameterName = dotnetParameter.Name
        }
    let createMethod (dotnetMethod : MethodInfo) =
        {
            Parameters = dotnetMethod.GetParameters() |> Array.toList |> List.map createParameter;
            ReturnType = Some (createTypeRef dotnetMethod.ReturnType);
            Name = dotnetMethod.Name
            IsStatic = dotnetMethod.IsStatic
            GenericParameters = 
                if dotnetMethod.ContainsGenericParameters
                then dotnetMethod.GetGenericArguments() |> List.ofArray |> List.map createGenericParameterInfo
                else []
        }
    let createConstructor (dotnetConstructor : ConstructorInfo) = 
        {
            Parameters = dotnetConstructor.GetParameters() |> Array.toList |> List.map createParameter;
        }
    let createField (dotnetField : FieldInfo) =
        {
            FieldName = dotnetField.Name
            TypeRef = createTypeRef dotnetField.FieldType
            IsStatic = dotnetField.IsStatic
            IsReadOnly = dotnetField.IsInitOnly || dotnetField.IsLiteral
            FieldDeclaringType = Identifier.fromDotNet dotnetField.DeclaringType
        }
    let createFieldFromProperty (dotnetProperty : PropertyInfo) =
        {
            FieldName = dotnetProperty.Name
            TypeRef = createTypeRef dotnetProperty.PropertyType
            IsStatic = dotnetProperty.GetMethod.IsStatic
            IsReadOnly = not dotnetProperty.CanWrite
            FieldDeclaringType = Identifier.fromDotNet dotnetProperty.DeclaringType
        }
    // if dotnetType.Name = "Enumerator" && dotnetType.DeclaringType.Name = "Dictionary`2"
    // then Debugger.Break()
    {
        BaseType = 
            if dotnetType.BaseType <> null then 
                Some (createTypeFromDotNetType dotnetType.BaseType)
            else None;
        DeclaredConstructors = 
            dotnetType.GetConstructors() 
            |> Array.toList 
            |> List.map createConstructor;
        Fields = 
            (dotnetType.GetFields() |> Array.toList |> List.map createField)
          @ (dotnetType.GetProperties() |> Array.toList |> List.map createFieldFromProperty)
        Identifier = Identifier.fromDotNet dotnetType
        GenericParameters = 
            if dotnetType.IsGenericParameter then 
                dotnetType.GetGenericParameterConstraints() 
                |> Array.toList 
                |> List.map createGenericParameterInfo
            else []
        ImplementedInterfaces = 
            dotnetType.GetInterfaces() 
            |> Array.toList 
            |> List.map createTypeFromDotNetType;
        Methods = 
            dotnetType.GetMethods() 
            |> Array.toList 
            |> List.map createMethod;
        NestedTypes = 
            dotnetType.GetNestedTypes() 
            |> Array.toList 
            |> List.map createTypeFromDotNetType
        IsStatic = dotnetType.IsAbstract && dotnetType.IsSealed
    }
let withNames = List.map (fun c -> (c.Identifier, c))
let typesFromAssembly (assembly : Assembly)= 
        assembly.GetExportedTypes()
        |> List.ofArray
        |> List.map (createTypeFromDotNetType)
        |> withNames
        |> Map.ofList

let getExternalTypes referencedAssemblies = 
    let types = 
        referencedAssemblies 
        |> List.fold (fun state assembly -> Map.union state (typesFromAssembly assembly)) Map.empty 
    types
