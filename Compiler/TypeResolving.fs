#if !(INTERACTIVE)
module Compiler.TypeResolving
#endif

#if INTERACTIVE
#load "Ast.fs"
#load "Types.fs"
#load "Result.fs"
#endif

open Compiler.Types
open Compiler.Ast
open Compiler.Result
open System
open System.Reflection

module ExternalTypes = 
    let rec createTypeFromDotNetType (dotnetType : System.Type) : Compiler.Types.Type = 
        let uniqueName (t : Type) = t.ToString()
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
            Name = dotnetType.ToString();
            GenericParameters = 
                if dotnetType.IsGenericParameter then 
                    dotnetType.GetGenericParameterConstraints() 
                    |> Array.map uniqueName 
                else [||]
            ImplementedInterfaces = dotnetType.GetInterfaces() |> (Array.map uniqueName);
            Methods = dotnetType.GetMethods() |> Array.map createMethod
        }

        let createUserDeclaredType declaration =
            let createMethod (method : Function) = 
                {
                    Parameters = dotnetMethod.GetParameters() |> Array.map createParameter;
                    ReturnType = uniqueName dotnetMethod.ReturnType;
                    MethodName = dotnetMethod.Name
                }
            let createClassDeclaration (classDeclaration : Class) =
                {
                    AssemblyName = "default"
                    BaseType = if dotnetType.BaseType <> null then 
                                   Some (uniqueName dotnetType.BaseType)
                               else None;
                    DeclaredConstructors = dotnetType.GetConstructors() |> Array.map createConstructor;
                    Fields = dotnetType.GetFields() |> Array.map createField;
                    Guid = dotnetType.GUID;
                    Name = dotnetType.ToString();
                    GenericParameters = 
                        if dotnetType.IsGenericParameter 
                        then dotnetType.GetGenericParameterConstraints() 
                             |> Array.map uniqueName 
                        else [||]
                    ImplementedInterfaces = dotnetType.GetInterfaces() |> (Array.map uniqueName);
                    Methods = dotnetType.GetMethods() |> Array.map createMethod
                }
            function 
            | FunctionDeclaration -> createMethodDeclaration 
            | ClassDeclaration -> createClassDeclaration

        let mscorlibTypes =
            let mscorlib = Assembly.GetAssembly(typeof<obj>)
            mscorlib.GetTypes()
            |> List.ofArray
            |> List.map (createTypeFromDotNetType >> (fun t -> (t.Name, t)))
            |> Map.ofList


        // let getBuiltInType =
        //         function 
        //         | Ast.IntLiteral(_) -> createBasicType "int"
        //         | Ast.FloatLiteral(_) -> createBasicType "float"
        //         | Ast.StringLiteral(_) -> createBasicType "string"
        //         | Ast.BoolLiteral(_) -> createBasicType "bool"

        // ExternalTypes.mscorlibTypes;;

module TypesScanner = 
    let scanAst scanType =
            let get = List.choose id 
            let scanExpression expression = 
                    match expression with
                    | NewExpression(t, args) -> [scanType t]
                    | _ -> []
            let rec scanStatement (statement : Statement) = 
                    match statement with
                    | FunctionCallStatement(FunctionCall(id, types, args))
                        -> types |> List.map scanType
                    | StaticFunctionCallStatement(t, FunctionCall(id,types,args))
                        -> scanType t :: (types |> List.map scanType)
                    | ValueDeclaration(id, typeSpec, expr)
                        -> get [typeSpec] |> List.map scanType
                    | VariableDeclaration(vd)
                        -> match vd with
                           | DeclarationWithType (i,t) -> [scanType t]
                           | FullDeclaration (i,t,e) -> [scanType t]
                           | DeclarationWithInitialization _ -> []
                    | CompoundStatement(cs)
                         -> cs |> List.collect scanStatement
                    | ReturnStatement e -> 
                         match e with 
                         | Some e -> scanExpression e
                         | None -> []
                    | AssignmentStatement (e1,e2) -> scanExpression e1 @ scanExpression e2

            let rec scanDeclaration = function
            | FunctionDeclaration(
                                    identifier,
                                    genericParameters,
                                    parameters,
                                    returnType,
                                    cs)
              -> ((get [returnType]) 
                 @ (parameters |> List.map snd)
              |> List.map scanType)
                 @ (cs |> List.collect scanStatement)
            | ClassDeclaration
                ({
                    Type = name;
                    GenericTypeParameters = typeParameters;
                    BaseTypes = baseTypes;
                    ValueDeclarations = values;
                    FieldDeclarations = variables;
                    Constructor = constructor;
                    FunctionDeclarations = methods})
              ->
               let scanConstructor = 
                   match constructor with
                   | None -> []
                   | Some c ->
                       let { Parameters = parameters; BaseClassConstructorCall = baseCall; Statements = stmts} = c
                       (parameters |> List.map (snd >> scanType)) @ (stmts |> List.collect scanStatement)

               (baseTypes |> List.map (CustomTypeSpec >> scanType))
                    @ (values |> List.collect (fun (i,t,e) -> get [t] |> List.map scanType))
                    @ (variables |> List.collect
                        (function
                         | DeclarationWithType (i,t) -> [scanType t]
                         | FullDeclaration (i,t,e) -> [scanType t]
                         | DeclarationWithInitialization _ -> []
                        )
                      )
                    @ scanConstructor
            List.map scanDeclaration

    let scanType 
            (typesDictionary : Map<string, Compiler.Types.Type>)
            (typeSpec : TypeSpec) =
            match typeSpec with
            | CustomTypeSpec (ns, CustomType(id, []))  ->
                    if typesDictionary.ContainsKey (typeSpec.ToString()) 
                    then succeedUnit
                    else fail (CannotResolveType typeSpec)
            | CustomTypeSpec (ns, CustomType(id, generics))  ->
                    let noGenericsTypeName = (CustomTypeSpec (ns, (CustomType(id, [])))).ToString()
                    let typeWithNumberOfGenerics = (noGenericsTypeName + "`" + (List.length generics).ToString() ) //TODO: get rid of this shit
                    match typesDictionary
                        |> Map.tryPick (fun key v -> if key.StartsWith(typeWithNumberOfGenerics) then Some v else None ) with
                    | Some v ->
                        if (generics |> List.map ((fun g -> g.ToString()) >> typesDictionary.ContainsKey)) |> List.forall ((=) true)
                        then succeedUnit
                        else fail (CannotResolveType typeSpec)
                    | None -> fail (CannotResolveType typeSpec)
            | _ -> succeedUnit

let scanTypes = TypesScanner.scanAst (TypesScanner.scanType ExternalTypes.mscorlibTypes)

