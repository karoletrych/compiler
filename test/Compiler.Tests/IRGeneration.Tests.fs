module Compiler.IRGeneration.Tests
open Expecto
open Compiler.Parser
open Compiler.CompilerResult
open Compiler.TypeIdentifiersFinding
open Compiler.TypeInference
open Compiler.TypeResolving
open Compiler.TypeFinding

// let findVariables src =
//     let externals = externalTypes [System.Reflection.Assembly.GetAssembly(typeof<obj>)]
//     [{Name = "test"; Code = src}]
//     |> parseModules 
//     >>= (fun modules -> resolve (modules, typeIdentifiers externals modules))
//     >>= (fun modules -> inferTypes (modules, typesDictionary externals modules))
//     |> Result.get
//     |> fun modules -> modules.Head.Functions.Head
//     |> fun f -> localVariables f.Body 
//                 |> List.map (fun v -> v.Name, v.Type.ToString())
        
// [<Tests>]
// let tests =
//   testList "IRGeneration.Tests" [
//     ftestCase "basic variables" <| fun _ ->
//         let variables = findVariables @"
//             fun main 
//             {
//                 val name = ""Karol"";
//                 val age = 22;
//                 val weight = 65.1;
//                 val arr = [1; 2; 3; ""string""; name; age; weight];
//             }"
//         Expect.equal 
//             variables 
//             [
//                 ("name", "System.String"); 
//                 ("age", "System.Int32"); 
//                 ("weight", "System.Single");
//                 ("arr", "Generic.Collections.System.List`1[System.Object]")
//             ] 
//             ""
//     ftestCase "nested if statements" <| fun _ ->
//         let variables = findVariables @"
//             fun main 
//             {
//                 val name = ""Karol"";
//                 val age = 22;
//                 if (age > 20)
//                 {
//                     var a = 3;
//                 }
//                 else if(age == 20)
//                 {
//                     var c = 1.0;
//                 }
//                 else
//                 {
//                     var d = true;
//                     if(name ==""Karol"")
//                     {
//                         var e = true;
//                     }
//                 }
//             }"
//         Expect.equal 
//             variables 
//                 [
//                     ("name", "System.String"); 
//                     ("age", "System.Int32"); 
//                     ("a", "System.Int32");
//                     ("c", "System.Single"); 
//                     ("d", "System.Boolean"); 
//                     ("e", "System.Boolean")
//                 ]
//             ""
// ]