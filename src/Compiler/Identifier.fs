namespace Compiler.Identifier
open Compiler.Ast

type Namespace = {
    Parts : string list
}

type TypeName = {
    Name : string list
    GenericArguments : TypeIdentifier list
}

and TypeIdentifier = {
    Namespace : string list
    TypeName : TypeName
} 
with member x.GenericArgumentsNumber = 
        x.TypeName.GenericArguments |> List.length
     override ti.ToString() =
       (ti.Namespace |> List.toArray |> (fun strs -> System.String.Join("::", strs)))
      + (ti.TypeName.Name |> List.toArray |> (fun strs -> System.String.Join("+",  strs)))
      + if List.isEmpty ti.TypeName.GenericArguments
        then ""
        else "`" + (List.length ti.TypeName.GenericArguments).ToString() + "[" + (ti.TypeName.GenericArguments |> List.map (fun x -> x.ToString()) |> String.concat ",") + "]"
    

module Identifier = 
    let rec fromTypeSpec (typeSpec : TypeSpec) = 
        let builtInTypeSpec =
            function
            | Bool -> {Namespace = ["System"]; TypeName = {Name = ["Bool"]; GenericArguments = []}} 
            | Char -> {Namespace = ["System"]; TypeName = {Name = ["Char"]; GenericArguments = []}} 
            | Int -> {Namespace = ["System"]; TypeName = {Name = ["Int32"]; GenericArguments = []}} 
            | Float -> {Namespace = ["System"]; TypeName = {Name = ["Single"]; GenericArguments = []}} 
            | Double -> {Namespace = ["System"]; TypeName = {Name = ["Double"]; GenericArguments = []}} 
            | String -> {Namespace = ["System"]; TypeName = {Name = ["String"]; GenericArguments = []}} 
            | Void -> {Namespace = ["System"]; TypeName = {Name = ["Void"]; GenericArguments = []}} 
            | Object  -> {Namespace = ["System"]; TypeName = {Name = ["Object"]; GenericArguments = []}} 
        match typeSpec with
        | BuiltInTypeSpec bits -> builtInTypeSpec bits
        | CustomTypeSpec cts -> 
        {
            Namespace = cts |> fst |> List.rev
            TypeName = 
            {
                Name = cts |> snd |> (fun t -> [t.Name]);
                GenericArguments = cts |> snd |> (fun t -> t.GenericArgs |> List.map fromTypeSpec)
            }
        } 
    let rec fromClassDeclaration (c : Class) = {
        Namespace = c.Name 
                    |> fun c -> c.Split([|"::"|], System.StringSplitOptions.None) 
                    |> List.ofArray
                    |> List.rev
                    |> List.tail
                    |> List.rev
        TypeName = 
        {
            Name =  [c.Name |> fun c -> c.Split([|"::"|], System.StringSplitOptions.None) 
                            |> List.ofArray
                            |> List.last]
            GenericArguments = [] // TODO: fix
        }
    } 

    let rec fromDotNet (t : System.Type) = {
        Namespace = t.Namespace |> fun ns -> ns.Split('.') |> List.ofArray |> List.rev
        TypeName = 
        {
            Name =  [ (if t.Name.Contains("`") then t.Name.Substring(0, t.Name.LastIndexOf("`")) else t.Name) ]
            GenericArguments = if t.IsGenericTypeDefinition then t.GetGenericArguments() |> List.ofArray |> List.map fromDotNet else []
        }
    } 
