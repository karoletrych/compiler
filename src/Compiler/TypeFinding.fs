#if !(INTERACTIVE)
module Compiler.TypeFinding
#endif

#if INTERACTIVE
#load "Ast.fs"
#load "Types.fs"
#load "Result.fs"
#endif

open FSharpx.Collections
open Compiler.Ast
open Compiler.Types
open Compiler.Identifier
open Compiler.ReferencedAssembliesMetadata
open Compiler.CompilerResult


let findTypesInModule (knownTypes : Type list) (modul : Module) =
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
let userDeclaredTypesWithKnownTypes knownTypes (modul : Module)  =
    findTypesInModule knownTypes modul
    |> withNames
    |> Map.ofList

let userDeclaredTypes (modul : Module) =
    userDeclaredTypesWithKnownTypes [] modul
    

open System
let allKnownTypes (modul : Module) =
    let referencedAssemblies = [Reflection.Assembly.GetAssembly(typeof<obj>)]
    let externalTypes =
        referencedAssemblies 
        |> List.fold (fun state assembly  -> Map.union state (typesFromAssembly assembly)) Map.empty //TODO: if there are 2 types with the same TypeIdentifier second one is chosen

    let userTypes =  userDeclaredTypesWithKnownTypes ((Map.values externalTypes) |> List.ofSeq) modul

    Map.union externalTypes userTypes |> Result.succeed


