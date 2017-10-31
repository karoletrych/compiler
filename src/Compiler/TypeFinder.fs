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
open Compiler.Identifier

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
    let createField (dotnetField : Reflection.FieldInfo) =
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

let findTypesInModule (knownTypes : Type list) (modul : Module.Module) =
    
    let rec createTypeFromClassDeclaration (declaredType : Class) =
        let getType (declaredType : TypeIdentifier) = 
            knownTypes
            |> List.tryFind (fun t -> t.Identifier = declaredType)
            |> Option.defaultWith (fun () -> 
                modul.Classes 
                |> List.find (fun c -> (Identifier.fromClassDeclaration c) = declaredType) 
                |> createTypeFromClassDeclaration)
        let createFunctionSignature (method : Ast.Function) = 
            {
                Parameters = 
                    method.Parameters 
                    |> List.map (fun (id, t) -> 
                    {
                        Type = fun() -> getType (Identifier.fromTypeSpec t);
                        ParameterName = id
                    });
                ReturnType = method.ReturnType |> Option.map (fun t -> fun () -> getType (Identifier.fromTypeSpec t))
                FunctionName = method.Name
            }
        let createConstructor (astCtor : Ast.Constructor) : Types.Constructor = 
            {
                Parameters = astCtor.Parameters 
                |> List.map (fun (name,t) -> 
                    {
                     ParameterName = name 
                     Type = fun() -> getType (Identifier.fromTypeSpec t);
                    });
            }
        {
            AssemblyName = "CHANGEIT"
            BaseType = (match declaredType.BaseClass with
                       | Some t -> Identifier.fromTypeSpec t
                       | None -> BuiltInTypeSpec Object |> Identifier.fromTypeSpec) 
                       |> getType |> Some;
            DeclaredConstructors = declaredType.Constructor |> Option.toList |> List.map createConstructor;
            Fields = declaredType.Properties 
                     |> List.map (fun property -> 
                         let name, t, _ = property 
                         (name, fun () -> t|> Identifier.fromTypeSpec |>  getType))
            Identifier = Identifier.fromClassDeclaration declaredType;
            GenericParameters = [] //TODO: fix
            GenericArguments = []
            ImplementedInterfaces = []
            Methods = declaredType.FunctionDeclarations |> List.map createFunctionSignature;
            NestedTypes = []
        }
    modul.Classes |> List.map createTypeFromClassDeclaration;

let withNames = List.map (fun c -> (c.Identifier, c))
let userDeclaredTypesWithKnownTypes knownTypes (modul : Module.Module)  =
    findTypesInModule knownTypes modul
    |> withNames
    |> Map.ofList

let userDeclaredTypes (modul : Module.Module) =
    userDeclaredTypesWithKnownTypes [] modul
    

//TODO: move to separate module "RetrieveReferencedAssembliesMetadata"
let mscorlibTypes = 
        Assembly.GetAssembly(typeof<obj>).GetExportedTypes()
        |> List.ofArray
        |> List.map (createTypeFromDotNetType)
        |> withNames
        |> Map.ofList
let allKnownTypes (modules : Module.Module) =
    let userTypes =  userDeclaredTypesWithKnownTypes ((Map.values mscorlibTypes) |> List.ofSeq) modules
    Map.union mscorlibTypes userTypes
