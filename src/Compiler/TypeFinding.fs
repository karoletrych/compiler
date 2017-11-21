module Compiler.TypeFinding
open Types
open Ast
open FSharpx.Collections

let private createFunctionSignature (method : Function<AstExpression>) = 
    {
        Parameters = 
            method.Parameters 
            |> List.map (fun (id, t) -> 
            {
                Type = Identifier.typeId t
                ParameterName = id
            });
        ReturnType = method.ReturnType |> Option.map Identifier.typeId
        Name = method.Name
    }
let private findTypesInModule (knownTypes : Map<TypeIdentifier, Type>) modul =
    let rec createTypeFromClassDeclaration declaredType =
        let getType (declaredType : TypeSpec) = 
            let typeId = Identifier.typeId declaredType    
            knownTypes
            |> Map.tryFind typeId
            |> Option.defaultWith(
                fun () ->
                    modul.Classes 
                    |> List.find (fun c -> (Identifier.fromClassDeclaration c) = typeId) 
                    |> createTypeFromClassDeclaration)
        
        let createConstructor (astCtor : Constructor<AstExpression>) = 
            {
                Parameters = astCtor.Parameters 
                |> List.map (fun (name,t) -> 
                    {
                     ParameterName = name 
                     Type = Identifier.fromTypeSpec t
                    });
            }
        {
            BaseType = (match declaredType.BaseClass with
                       | Some t -> t
                       | None -> TypeIdentifier Identifier.object)
                       |> getType |> Some;
            DeclaredConstructors = declaredType.Constructor |> Option.toList |> List.map createConstructor;
            Fields = declaredType.Properties 
                     |> List.map (fun prop -> 
                         (prop.Name, Identifier.fromTypeSpec prop.Type))
            Identifier = Identifier.fromClassDeclaration declaredType;
            GenericParameters = [] //TODO: fix
            GenericArguments = []
            ImplementedInterfaces = []
            Methods = declaredType.Functions |> List.map createFunctionSignature;
            NestedTypes = []
        }
    modul.Classes 
        |> List.map createTypeFromClassDeclaration

let moduleType modul = 
    {
        BaseType = None
        DeclaredConstructors = []
        Fields = []
        Identifier = Identifier.fromModule modul
        GenericParameters = [] 
        GenericArguments = []
        ImplementedInterfaces = []
        Methods = modul.Functions |> List.map createFunctionSignature;
        NestedTypes = []
    }
let typesDictionary externalTypes modules =
    let withNames = List.map (fun c -> (c.Identifier, c)) >> Map.ofList
    modules    
    |> List.collect (fun m -> (moduleType m) :: findTypesInModule externalTypes m)
    |> withNames
    |> Map.union externalTypes
