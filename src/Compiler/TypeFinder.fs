#if !(INTERACTIVE)
module Compiler.TypeFinder
#endif

#if INTERACTIVE
#load "Ast.fs"
#load "Types.fs"
#load "Result.fs"
#endif

open System
open System.Reflection
open FSharpx.Collections
open Compiler.Ast
open Compiler.Types

let rec createTypeFromDotNetType (dotnetType : System.Type) : Types.Type = 
    let uniqueName (t : System.Type) = t.ToString()
    let createParameter (dotnetParameter : ParameterInfo) = 
        {
            Type = uniqueName dotnetParameter.ParameterType;
            ParameterName = dotnetParameter.Name
        }
    let createMethod (dotnetMethod : MethodInfo) =
        {
            Parameters = dotnetMethod.GetParameters() |> Array.toList|> List.map createParameter;
            ReturnType = Some (uniqueName dotnetMethod.ReturnType);
            FunctionName = dotnetMethod.Name
        }
    let createConstructor (dotnetConstructor : ConstructorInfo) = 
        {
            Parameters = dotnetConstructor.GetParameters() |> Array.toList|> List.map createParameter;
        }
    let createField (dotnetField : Reflection.FieldInfo) =
        dotnetField.Name, uniqueName dotnetField.FieldType
    {
        AssemblyName = dotnetType.AssemblyQualifiedName;
        BaseType = if dotnetType.BaseType <> null then 
                       Some (uniqueName dotnetType.BaseType)
                   else None;
        DeclaredConstructors = dotnetType.GetConstructors() |> Array.toList|> List.map createConstructor;
        Fields = dotnetType.GetFields() |> Array.toList|> List.map createField;
        Name = dotnetType.ToString();
        GenericParameters = 
            if dotnetType.IsGenericParameter then 
                dotnetType.GetGenericParameterConstraints() 
                |> Array.toList |> List.map (fun x->x.ToString())
            else []
        ImplementedInterfaces = dotnetType.GetInterfaces() |> Array.toList |> List.map uniqueName;
        Methods = dotnetType.GetMethods() |> Array.toList|> List.map createMethod;
        NestedTypes = dotnetType.GetNestedTypes() |> Array.toList |> List.map uniqueName 
    }

let findTypesInModule (modul : Module.Module) =
    let typeName (t: TypeSpec) : TypeName = t.ToString()
    
    let createClassDeclaration (declaredType : Ast.Class) =
        let createParameter astParameter = 
            {
                ParameterName = fst astParameter 
                Type = snd astParameter |> typeName;
            }
        let createConstructor (astCtor : Ast.Constructor) : Types.Constructor = 
            {
                Parameters = astCtor.Parameters |> List.map createParameter;
            }
        {
            AssemblyName = "CHANGEIT"
            BaseType = (match declaredType.BaseClass with
                        | Some t -> t |> CustomTypeSpec 
                        | None -> Object)
                        |> typeName |> Some
            DeclaredConstructors = declaredType.Constructor |> Option.toList |> List.map createConstructor;
            Fields = declaredType.Properties 
                     |> List.map (fun property -> 
                         let name, t, _ = property
                         (name, typeName t))
            Name = declaredType.Name;
            GenericParameters = declaredType.GenericTypeParameters |> List.map (fun (GenericTypeParameter(x)) -> x)
            ImplementedInterfaces = declaredType.ImplementedInterfaces |> List.map (CustomTypeSpec >> typeName);
            Methods = declaredType.FunctionDeclarations |> List.map Function.createFunctionDeclaration;
            NestedTypes = []
        }
    modul.Classes |> List.map createClassDeclaration;

let withNames = List.map (fun c -> (c.Name, c))
let userDeclaredTypes (modul : Module.Module)  =
    let classesFromModule =
        findTypesInModule modul
    classesFromModule 
    |> withNames
    |> Map.ofList

    
let mscorlibTypes = 
        Assembly.GetAssembly(typeof<obj>).GetTypes()
        |> List.ofArray
        |> List.map (createTypeFromDotNetType)
        |> withNames
        |> Map.ofList

let allKnownTypes (modules : Module.Module) =
    let userTypes = modules |> userDeclaredTypes
    Map.union mscorlibTypes userTypes
