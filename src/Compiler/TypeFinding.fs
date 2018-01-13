/// this module takes list of modules retrieved from parser and
/// generates Map<TypeIdentifier, Types.Type> 
/// it flattens the structure
/// the result contains modules and types within them

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
                Type = ConstructedType (Identifier.typeId t)
                ParameterName = id
            });
        ReturnType = method.ReturnType |> Option.map (Identifier.typeId >> ConstructedType)
        Name = method.Name
        IsStatic = isStatic
        GenericParameters = []
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
                     Type = Identifier.fromTypeSpec t |> ConstructedType
                    });
            }
        let baseType = 
            (match declaredType.BaseClass with
                       | Some t -> t
                       | None -> TypeIdentifier Identifier.object)
                       |> getType
        {
            BaseType = Some baseType;
            DeclaredConstructors = declaredType.Constructors |> List.map createConstructor;
            Fields = declaredType.Fields 
                     |> List.map (fun field -> 
                         ({FieldName = field.Name; 
                           TypeRef = Identifier.fromTypeSpec field.Type |> ConstructedType; 
                           IsStatic = false
                           IsReadOnly = field.IsReadOnly}))
            Identifier = declaredType.Identifier;
            GenericParameters = []
            ImplementedInterfaces = []
            Methods = (declaredType.Functions |> List.map (createFunctionSignature false))
                    @ baseType.Methods
            NestedTypes = []
            IsStatic = false
        }
    modul.Classes 
        |> List.map createTypeFromClassDeclaration

let moduleType (modul : Module<AstExpression>) classes : Types.Type = 
    {
        BaseType = None
        DeclaredConstructors = []
        Fields = []
        Identifier = modul.Identifier
        GenericParameters = [] 
        ImplementedInterfaces = []
        Methods = modul.Functions |> List.map (createFunctionSignature true)
        NestedTypes = classes
        IsStatic = true
    }
let find externalTypes (modules : Module<AstExpression> list) =
    let withNames (types : Type list) = 
        types |> (List.map (fun c -> (c.Identifier, c)) >> Map.ofList)
    modules    
    |> List.collect (fun m -> 
        moduleType m (findTypesInModule externalTypes m) 
     :: findTypesInModule externalTypes m)
    |> withNames
    |> Map.union externalTypes
