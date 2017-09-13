#if !(INTERACTIVE)
module Compiler.TypeResolving
#endif

#if INTERACTIVE
#load @"Ast.fs"
#load @"Types.fs"
#endif

open Compiler.Types
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

        let getMscorlibTypes =
                let mscorlib = Assembly.GetAssembly(typeof<obj>)
                mscorlib.GetTypes()
                |> Array.map (createTypeFromDotNetType Map.empty)

// let getBuiltInType =
//         function 
//         | Ast.IntLiteral(_) -> createBasicType "int"
//         | Ast.FloatLiteral(_) -> createBasicType "float"
//         | Ast.StringLiteral(_) -> createBasicType "string"
//         | Ast.BoolLiteral(_) -> createBasicType "bool"

ExternalTypes.getMscorlibTypes;;
