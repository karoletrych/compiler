module Compiler.ReferencedAssembliesMetadata

open System.Reflection
open Ast
open Types
open FSharpx.Collections
let rec createTypeFromDotNetType (dotnetType : System.Type) : Types.Type = 
    let createGenericParameterInfo (dotnetType : System.Type) = 
        let position = dotnetType.GenericParameterPosition
        let declared = 
            if dotnetType.DeclaringMethod |> isNull
            then
                Class (Identifier.fromDotNet dotnetType.DeclaringType)
            else
                Method ((Identifier.fromDotNet dotnetType.DeclaringMethod.DeclaringType), dotnetType.DeclaringMethod.Name)
        (declared, position)
    let createTypeRef (dotnetType : System.Type) : TypeRef= 
        if dotnetType.IsGenericParameter
        then GenericParameter (createGenericParameterInfo dotnetType)
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
            Type = createTypeRef dotnetField.FieldType
            IsStatic = dotnetField.IsStatic
        }
    let createFieldFromProperty (dotnetProperty : PropertyInfo) =
        {
            FieldName = dotnetProperty.Name
            Type = createTypeRef dotnetProperty.PropertyType
            IsStatic = dotnetProperty.GetMethod.IsStatic
        }
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

let externalTypes referencedAssemblies = 
    let types = //TODO: if there are 2 types with the same TypeIdentifier second one is chosen
        referencedAssemblies 
        |> List.fold (fun state assembly -> Map.union state (typesFromAssembly assembly)) Map.empty 
    types
