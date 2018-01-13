module Compiler.IRGeneration.Tests
open Expecto
open Compiler.Parser
open Compiler.CompilerResult
open Compiler.TypeInference
open Compiler.TypeResolving
open Compiler
open Compiler.Ast

let findVariables src =
    let externals = ReferencedDllsMetadataRetriever.getExternalTypes [System.Reflection.Assembly.GetAssembly(typeof<obj>)]
    [{Path = "test"; Code = src}]
    |> parseModules 
    >>= (fun modules -> resolve (modules, externals))
    >>= (fun modules -> inferTypes (modules, TypeFinding.find externals modules))
    |> Result.get
    |> fun modules -> (fst modules).Head.Functions.Head
    |> fun f -> IRGeneration.findLocalVariables f.Body 
                |> List.map (fun v -> v.Name, v.TypeId)
        
[<Tests>]
let tests =
  testList "IRGeneration.Tests" [
    ftestCase "basic variables" <| fun _ ->
        let variables = findVariables @"
            fun main 
            {
                val name = ""Karol"";
                val age = 22;
                val weight = 65.1;
                val arr = [1; 2; 3; ""string""; name; age; weight];
            }"
        Expect.equal variables [("arr", {Namespace = ["Generic"; "Collections"; "System"];
          Name = "List`1";
          GenericParameters = [GenericArgument {Namespace = ["System"];
                                                Name = "Object";
                                                GenericParameters = [];
                                                DeclaringType = None;}];
          DeclaringType = None;}); ("weight", {Namespace = ["System"];
                                               Name = "Single";
                                               GenericParameters = [];
                                               DeclaringType = None;});
 ("age", {Namespace = ["System"];
          Name = "Int32";
          GenericParameters = [];
          DeclaringType = None;}); ("name", {Namespace = ["System"];
                                             Name = "String";
                                             GenericParameters = [];
                                             DeclaringType = None;})] ""
    ftestCase "nested if statements" <| fun _ ->
        let variables = findVariables @"
            fun main 
            {
                val name = ""Karol"";
                val age = 22;
                if (age > 20)
                {
                    var a = 3;
                }
                else if(age == 20)
                {
                    var c = 1.0;
                }
                else
                {
                    var d = true;
                    if(name ==""Karol"")
                    {
                        var e = true;
                    }
                }
            }"
        Expect.equal variables 
                [
                    
                ]
            ""
]