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
open Compiler.Ast
open Compiler.Types
open Compiler.Result

let rec createTypeFromDotNetType (dotnetType : System.Type) : Compiler.Types.Type = 
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
        Guid = dotnetType.GUID;
        Name = dotnetType.ToString();
        GenericParameters = 
            if dotnetType.IsGenericParameter then 
                dotnetType.GetGenericParameterConstraints() 
                |> Array.toList |> List.map (fun x->x.ToString())
            else []
        ImplementedInterfaces = dotnetType.GetInterfaces() |> Array.toList |> List.map uniqueName;
        Methods = dotnetType.GetMethods() |> Array.toList|> List.map createMethod
    }



let createUserDeclaredModule ast name =
    let typeName (t: TypeSpec) : TypeName = "DUPA.888"
    let createFunctionDeclaration (method : Ast.Function) = 
        {
            Parameters = method.Parameters |> List.map (fun (id, t) -> {Type = typeName t; ParameterName = id});
            ReturnType = method.ReturnType |> Option.map typeName
            FunctionName = method.Name
        }
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
            AssemblyName = "default"
            BaseType = declaredType.BaseClass |> Option.map (CustomTypeSpec >> typeName)
            DeclaredConstructors = declaredType.Constructor |> Option.toList |> List.map createConstructor;
            Fields = declaredType.Properties 
                     |> List.map (fun property -> 
                         let name, t, _ = property
                         (name, typeName t))
            Guid = Guid.NewGuid();
            Name = declaredType.Name;
            GenericParameters = declaredType.GenericTypeParameters |> List.map (fun (GenericTypeParameter(x)) -> x)
            ImplementedInterfaces = declaredType.ImplementedInterfaces |> List.map (CustomTypeSpec >> typeName);
            Methods = declaredType.FunctionDeclarations |> List.map createFunctionDeclaration
        }
    let functions = List.choose (fun elem ->
        match elem with
        | FunctionDeclaration f  -> Some f
        | _ -> None) 
    let classes = List.choose (fun elem ->
        match elem with
        | ClassDeclaration c  -> Some c
        | _ -> None) 
    {
        Functions = ast |> functions |> List.map createFunctionDeclaration;
        Classes = ast |> classes |> List.map createClassDeclaration;
        Name = name
    }

// TODO: differentiate between instance and static functions
let createModuleType name functions =
    {
            AssemblyName = "default"
            BaseType = None;
            DeclaredConstructors = []
            Fields = []
            Guid = Guid.NewGuid();
            Name = name
            GenericParameters = [];
            ImplementedInterfaces = []
            Methods = functions
    }

let withNames = List.map (fun c -> (c.Name, c))
let userDeclaredTypes (modules : Tuple<string, Declaration list> list) =
    let userDeclaredModules =
        modules 
        |> List.map (fun m -> createUserDeclaredModule (snd m) (fst m)) 
    let userTypes = 
        userDeclaredModules 
        |> List.collect (fun m -> (m.Classes |> withNames))
    let userDeclaredModuleTypes = 
        userDeclaredModules
        |> List.map (fun m -> createModuleType m.Name m.Functions)
        |> withNames
    List.append userTypes userDeclaredModuleTypes
    
let mscorlibTypes = 
        Assembly.GetAssembly(typeof<obj>).GetTypes()
        |> List.ofArray
        |> List.map (createTypeFromDotNetType)
        |> withNames

let types (modules : Tuple<string, Declaration list> list) =
    List.append mscorlibTypes (userDeclaredTypes modules)
