module Compiler.TypeFinding
open Types
open Ast
open FSharpx.Collections

let private createFunctionSignature isStatic (method : Function<AstExpression>)  : Types.Function = 
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
        IsStatic = isStatic
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
                    |> List.find (fun c -> c.Identifier = typeId) 
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
                     |> List.map (fun field -> 
                         ({FieldName = field.Name; Type = Identifier.fromTypeSpec field.Type; IsStatic = false}))
            Identifier = declaredType.Identifier;
            GenericParameters = []
            GenericArguments = []
            ImplementedInterfaces = []
            Methods = declaredType.Functions 
                      |> List.map (createFunctionSignature false)
            NestedTypes = []
            IsStatic = false
        }
    modul.Classes 
        |> List.map createTypeFromClassDeclaration

let moduleType (modul : Module<AstExpression>) = 
    {
        BaseType = None
        DeclaredConstructors = []
        Fields = []
        Identifier = modul.Identifier
        GenericParameters = [] 
        GenericArguments = []
        ImplementedInterfaces = []
        Methods = modul.Functions |> List.map (createFunctionSignature true);
        NestedTypes = []
        IsStatic = true
    }
let typesDictionary externalTypes (modules : Module<AstExpression> list) =
    let withNames (types : Type list) = 
        types |> (List.map (fun c -> (c.Identifier, c)) >> Map.ofList)
    modules    
    |> List.collect (fun m -> (moduleType m) :: findTypesInModule externalTypes m)
    |> withNames
    |> Map.union externalTypes
