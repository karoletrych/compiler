module Compiler.TypeResolving

open Compiler.Types
open Compiler.Ast
open System
open System.Reflection

module ExternalTypes = 
        let rec createTypeFromDotNetType (types : Map<string, Type>) (dotnetType : System.Type) = 
                let uniqueName (t : Type) = t.AssemblyQualifiedName
                let createType newType = createTypeFromDotNetType (types.Add(newType))
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
                    Name = dotnetType.Name;
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
                |> Array.map (createTypeFromDotNetType Map.empty)


#if INTERACTIVE
#load @"Ast.fs"
#load @"Types.fs"
#endif

let scanAst scanType =
        let get = List.choose id 
        let scanGenericType = () //TODO: implement
        let rec scanDeclaration = function
        | FunctionDeclaration(
                                identifier,
                                genericParameters,
                                parameters,
                                returnType,
                                cs)
          -> (get [returnType]) 
                @ (parameters |> List.map snd)
          |> List.map scanType
        | ClassDeclaration({
                              Type = name;
                              GenericTypeParameters = typeParameters;
                              BaseTypes = baseTypes;
                              ValueDeclarations = values;
                              FieldDeclarations = variables;
                              Constructor = constructor;
                              FunctionDeclarations = methods})
          -> (baseTypes |> List.map (CustomTypeSpec >> scanType))
                @ (values |> List.map (fun (i,t,e) -> t) |> get)
                @ (variables |> List.collect
                                (function
                                 | DeclarationWithType (i,t) -> [t]
                                 | FullDeclaration (i,t,e) -> [t]
                                 | DeclarationWithInitialization _ -> []))
                |> List.map scanType
        List.map scanDeclaration

// let getBuiltInType =
//         function 
//         | Ast.IntLiteral(_) -> createBasicType "int"
//         | Ast.FloatLiteral(_) -> createBasicType "float"
//         | Ast.StringLiteral(_) -> createBasicType "string"
//         | Ast.BoolLiteral(_) -> createBasicType "bool"

// ExternalTypes.mscorlibTypes;;