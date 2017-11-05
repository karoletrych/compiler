module Compiler.ReferencedAssembliesMetadata
open System.Reflection
open Compiler.Ast
open Compiler.Types
open FSharpx.Collections

let rec createTypeFromDotNetType (dotnetType : System.Type) : Types.Type = 
    let createParameter (dotnetParameter : ParameterInfo) = 
        {
            Type = fun () -> createTypeFromDotNetType dotnetParameter.ParameterType;
            ParameterName = dotnetParameter.Name
        }
    let createMethod (dotnetMethod : MethodInfo) =
        {
            Parameters = dotnetMethod.GetParameters() |> Array.toList|> List.map createParameter;
            ReturnType = Some (fun () -> createTypeFromDotNetType dotnetMethod.ReturnType);
            FunctionName = dotnetMethod.Name
        }
    let createConstructor (dotnetConstructor : ConstructorInfo) = 
        {
            Parameters = dotnetConstructor.GetParameters() |> Array.toList|> List.map createParameter;
        }
    let createField (dotnetField : FieldInfo) =
        dotnetField.Name, fun () -> createTypeFromDotNetType dotnetField.FieldType
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
                |> List.map (fun x -> fun () -> createTypeFromDotNetType x)
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

let exportedTypes referencedAssemblies = 
    //TODO: if there are 2 types with the same TypeIdentifier second one is chosen
    referencedAssemblies 
    |> List.fold (fun state assembly  -> Map.union state (typesFromAssembly assembly)) Map.empty 
