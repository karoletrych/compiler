module Compiler.TypeInference

open Compiler.Ast
open Compiler.Types
open CompilerResult

// TODO: consider: type Inferred = Inferred of Expression * TypeIdentifier option

let builtInType (types : Map<TypeIdentifier, Type>) (t : TypeSpec) =
    types.[t |> Identifier.fromTypeSpec]

let leastUpperBound (knownTypes : Map<TypeIdentifier, Type>) types=
    let rec allAncestors (t : Type) : Type list  =
        match t.BaseTypes with 
        | [] -> [] 
        | b -> b @ (b |> List.collect (fun t -> (allAncestors t)))

    let children allNodes parentNode = 
                allNodes 
                |> List.filter (fun (t : Type) -> 
                    t.BaseTypes |> List.contains parentNode
                    )

    let rec longestPath (root : Type) allNodes =
        let childrenOf = children allNodes
        let isALeaf = childrenOf root = List.empty
        if isALeaf 
        then
            ([root], 0)
        else
            let edges = childrenOf root |> List.map (fun child ->
                let (p, l) = longestPath child allNodes
                (child :: p, l + 1)
                )
            edges |> List.maxBy snd

    let longest = longestPath (builtInType knownTypes (BuiltInTypeSpec Object))

    types
        |> List.map (fun t -> (t::allAncestors t))
        |> List.map Set.ofList
        |> Set.intersectMany
        |> Set.toList
        |> List.distinct
        |> longest
        |> fst
        |> List.last

// TODO: split into function composition
let inferTypes (modul : Module, knownTypes : Map<TypeIdentifier, Type>) = 
    let rec inferExpressionType 
        (c: Ast.Class) 
        (declaredVariables : Map<string, Types.Type>) 
        (expression : Ast.Expression) 
            : Types.Type option =

        let lookupLocalVariable (declaredVariables : Map<string, Type>) identifier: Types.Type option =
            Some declaredVariables.[identifier]
        let lookupDeclaredFunctions (parentClass : Class) identifier : Types.Type option =
            parentClass.FunctionDeclarations 
            |> List.tryFind (fun f -> f.Name = identifier) 
            |> Option.map (fun f -> f.ReturnType)
            |> Option.bind (fun t -> match t with 
                                     | Some t -> knownTypes.TryFind (t |> Identifier.fromTypeSpec)
                                     | None -> None)
        let lookupMemberFunctions identifier : Types.Type option =
            failwith "not implemented"
        let lookupStaticFunctionReturnType staticCall =
            failwith "not implemented"
        let inferBinaryExpressionType expr1Type op expr2Type = 
            let someIfEqual e1 e2 = if e1 = e2 then Some e1 else None
            match op,expr1Type,expr2Type with
            | _, None, _ -> None
            | _, _, None -> None
            | Plus, Some e1, Some e2 -> someIfEqual e1 e2
            | Minus, Some e1, Some e2 -> someIfEqual e1 e2
            | Multiplication, Some e1, Some e2 -> someIfEqual e1 e2
            | Division, Some e1, Some e2 -> someIfEqual  e1 e2
            | Remainder, Some e1, Some e2 -> someIfEqual e1 e2
            | _ -> Some (builtInType knownTypes (BuiltInTypeSpec Bool))

        let typeOfLiteral = 
            (function
            | BoolLiteral _-> builtInType knownTypes (BuiltInTypeSpec Bool);
            | IntLiteral _-> builtInType knownTypes (BuiltInTypeSpec Int);
            | FloatLiteral _-> builtInType knownTypes (BuiltInTypeSpec Float);
            | StringLiteral _-> builtInType knownTypes (BuiltInTypeSpec String);)
            >> Some
        let leastUpperBound = leastUpperBound knownTypes

        let lookupIdentifier = lookupLocalVariable declaredVariables
        let inferExpressionType = inferExpressionType c declaredVariables 
        let lookupListType types =
            if List.forall (fun (t : Type option) -> t.IsSome) types 
            then Some (leastUpperBound (types |> List.map (fun x-> x.Value))) 
            else None

        match expression with
        | AssignmentExpression(_, e) -> inferExpressionType e
        | BinaryExpression(e1, op, e2) ->
            inferBinaryExpressionType (inferExpressionType e1) op (inferExpressionType e2)
        | InferredTypeExpression _ -> failwith "unexpected expression with inferred type"
        | FunctionCallExpression(fc) -> lookupDeclaredFunctions c (fc.Name)
        | IdentifierExpression(ie) -> lookupIdentifier ie 
        | LiteralExpression(le) -> typeOfLiteral le
        | ListInitializerExpression list -> lookupListType (list |> List.map inferExpressionType)
        | MemberExpression mfc -> lookupMemberFunctions mfc
        | NewExpression (t, _) -> knownTypes.TryFind (t |> Identifier.fromTypeSpec)
        | StaticMemberExpression (t, call) -> lookupStaticFunctionReturnType (t,call)
        | UnaryExpression(_, e) -> inferExpressionType e

    let annotateExpression c variables e =
        let exprType = inferExpressionType c variables e
        InferredTypeExpression(e, exprType |> Option.map (fun e -> e.Identifier.ToString()))

    let rec annotateStatement 
        (c: Ast.Class) 
        (declaredVariables : Map<string, Type>) 
        (statement : Ast.Statement)
        : (Statement * Map<string, Type>) = 
        let annotateStatement = annotateStatement c
        let annotateExpression = annotateExpression c declaredVariables
        let inferExpressionType = inferExpressionType c declaredVariables
        
        match statement with
        | AssignmentStatement (e1, e2) ->
            AssignmentStatement(annotateExpression e1, annotateExpression e2), declaredVariables
        | BreakStatement ->
            BreakStatement, declaredVariables
        | CompositeStatement cs ->
            (cs 
            |> List.map ((fun s -> annotateStatement declaredVariables s) >> fst)
            |> CompositeStatement) , declaredVariables
        | FunctionCallStatement fcs ->
            FunctionCallStatement fcs, declaredVariables
        | IfStatement (e, s, elseS) ->
            IfStatement(e,
             annotateStatement declaredVariables s |> fst,
              elseS |> Option.map (fun elseS -> annotateStatement declaredVariables elseS |> fst) ),
               declaredVariables
        | MemberFunctionCallStatement mfcs ->
            MemberFunctionCallStatement mfcs, declaredVariables
        | ReturnStatement exprOption ->
            exprOption
            |> Option.map annotateExpression
            |> ReturnStatement, declaredVariables
        | StaticFunctionCallStatement (t,c) ->
            StaticFunctionCallStatement (t,c), declaredVariables
        | ValueDeclaration(id, t, expr) ->
            ValueDeclaration(id, t, annotateExpression expr),

            let valueType = 
                        t 
                        |> Option.map (fun t -> knownTypes.[t |> Identifier.fromTypeSpec])
                        |> Option.orElse (
                             (inferExpressionType expr) |> Option.map (fun t -> knownTypes.[t.Identifier]))
            match valueType with
            | Some t -> declaredVariables |> Map.add id t
            | None -> declaredVariables
        | VariableDeclaration vd ->
            (
            match vd with
            | DeclarationWithInitialization (id, e) ->
                VariableDeclaration(DeclarationWithInitialization(id, annotateExpression e)),
                match inferExpressionType e with
                | Some t -> declaredVariables |> Map.add id t
                | None -> declaredVariables 
            | DeclarationWithType(id, t) ->
                VariableDeclaration(DeclarationWithType(id, t)),
                declaredVariables |> Map.add id knownTypes.[t |> Identifier.fromTypeSpec] // TODO: exception (?)
            | FullDeclaration (id, t, e) ->
                VariableDeclaration(FullDeclaration(id, t, annotateExpression e)),
                declaredVariables |> Map.add id knownTypes.[t |> Identifier.fromTypeSpec] // TODO: ^ 
            )
        | WhileStatement (e,s) -> 
            WhileStatement(annotateExpression e, s), declaredVariables
    let inferFunction (c : Ast.Class) (f: Ast.Function) = 
        let annotateStatement = annotateStatement c
        let declaredVariables =
            f.Parameters
            |> List.map (fun p -> (fst p, knownTypes.[(snd p) |> Identifier.fromTypeSpec]))
            |> Map.ofList
        f.Body
        |> List.mapFold annotateStatement declaredVariables
        |> fst

    modul.Classes
    |> List.map (fun c -> 
        {
            c with
                FunctionDeclarations =
                    c.FunctionDeclarations |> List.map (fun f -> {f with Body = inferFunction c f})
        })
    |> fun c -> Result.succeed {Classes = c}
        
