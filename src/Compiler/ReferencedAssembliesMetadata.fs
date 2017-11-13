module Compiler.ReferencedAssembliesMetadata

open System.Reflection
open Ast
open Types
open FSharpx.Collections

let rec createTypeFromDotNetType (dotnetType : System.Type) : Types.Type = 
    let createParameter (dotnetParameter : ParameterInfo) = 
        {
            Type = Identifier.fromDotNet dotnetParameter.ParameterType;
            ParameterName = dotnetParameter.Name
        }
    let createMethod (dotnetMethod : MethodInfo) =
        {
            Parameters = dotnetMethod.GetParameters() |> Array.toList|> List.map createParameter;
            ReturnType = Some (Identifier.fromDotNet dotnetMethod.ReturnType);
            Name = dotnetMethod.Name
        }
    let createConstructor (dotnetConstructor : ConstructorInfo) = 
        {
            Parameters = dotnetConstructor.GetParameters() |> Array.toList|> List.map createParameter;
        }
    let createField (dotnetField : FieldInfo) =
        dotnetField.Name, Identifier.fromDotNet dotnetField.FieldType
    {
        AssemblyName = dotnetType.AssemblyQualifiedName;
        BaseType = 
            if dotnetType.BaseType <> null then 
                Some (createTypeFromDotNetType dotnetType.BaseType)
            else None;
        DeclaredConstructors = dotnetType.GetConstructors() |> Array.toList|> List.map createConstructor;
        Fields = dotnetType.GetFields() |> Array.toList |> List.map createField;
        Identifier = Identifier.fromDotNet dotnetType
        GenericParameters = 
            if dotnetType.IsGenericParameter then 
                dotnetType.GetGenericParameterConstraints() 
                |> Array.toList 
                |> List.map Identifier.fromDotNet
            else []
        GenericArguments = []
        ImplementedInterfaces = dotnetType.GetInterfaces() |> Array.toList |> List.map createTypeFromDotNetType;
        Methods = dotnetType.GetMethods() |> Array.toList|> List.map createMethod;
        NestedTypes = dotnetType.GetNestedTypes() |> Array.toList |> List.map createTypeFromDotNetType
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
