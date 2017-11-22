module Compiler.IntegrationTests

open Expecto
open System.IO
open System.Reflection
open System.Reflection.Emit
open System
open Compiler
open Parser
open CILGeneration
open Compiler.CompilerResult
open CILGeneration

let files = 
    System.IO.Directory.GetFiles(@".\test\Compiler.IntegrationTests\tests") 
    |> List.ofArray
let testFiles = 
    files 
    |> List.filter (fun f -> Path.GetExtension(f) = ".ks") 

let resultFiles = 
    files 
    |> List.filter (fun f -> Path.GetExtension(f) = ".out") 
let withCorrespondingOutput input = 
    input, resultFiles |> List.find (fun f -> 
                        Path.GetFileNameWithoutExtension(f) = Path.GetFileNameWithoutExtension(input))

let readFiles (input, output) = ({Name = input; Code = File.ReadAllText input}, File.ReadAllText output)
let mscorlib = [Assembly.GetAssembly(typeof<obj>)]
let compile input = Compiler.compile [input] mscorlib
let generateAssembly (ir : CompilerResult<IR.Module list>) = 
    let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(
                            AssemblyName("Test.exe"), AssemblyBuilderAccess.RunAndSave)
    generateAssembly assemblyBuilder mscorlib (Result.get ir) true
    assemblyBuilder

let execute (assemblyBuilder : AssemblyBuilder) =
    use stringWriter = new StringWriter()
    let setOut = assemblyBuilder.GetType("SystemConsole").GetMethod("SetOut");
    setOut.Invoke(null, [|stringWriter|]) |> ignore

    assemblyBuilder.EntryPoint.Invoke(null, Array.empty) |> ignore

    stringWriter.ToString()

let tests1 =
    testFiles
    |> List.map (
            withCorrespondingOutput 
            >> readFiles
            >> fun (input, expectedOutput) -> 
                input |> compile |> generateAssembly |> execute, expectedOutput
    )

[<Tests>]
let tests =
  testList "Integration tests" [
    testCase "create from class declaration" <| fun _ ->
        Expect.equal files [] ""
]

[<EntryPoint>]
let main argv =
    runTestsInAssembly defaultConfig argv
