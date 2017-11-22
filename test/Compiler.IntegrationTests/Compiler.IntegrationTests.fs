module Compiler.IntegrationTests

open Expecto
open System.IO
open System.Reflection
open System.Reflection.Emit
open System
open Compiler
open Parser
open CILGeneration
open CompilerResult
let testDirectory = Path.Combine(Assembly.GetEntryAssembly().Location, @"..\..\..\..\tests")
let files = 
    System.IO.Directory.GetFiles(testDirectory)
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
let compile input = compile [input] mscorlib
let generateAssembly (ir : CompilerResult<IR.Module list>) = 
    let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(
                            AssemblyName("Test.exe"), AssemblyBuilderAccess.RunAndSave)
    generateAssembly assemblyBuilder mscorlib (Result.get ir) true
    assemblyBuilder

let execute (assemblyBuilder : AssemblyBuilder) =
    use stringWriter = new StringWriter()
    let originalOut = Console.Out
    Console.SetOut(stringWriter)

    assemblyBuilder.EntryPoint.Invoke(null, Array.empty) |> ignore

    Console.SetOut(originalOut)
    stringWriter.ToString()

let getTestData = 
    withCorrespondingOutput 
    >> readFiles
    >> fun (input, expectedOutput) -> 
            input |> compile |> generateAssembly |> execute, expectedOutput

let createTest testName (output, expectedOutput) = 
    testCase testName (fun _ -> Expect.equal output expectedOutput testName)


[<Tests>]
let tests = 
    testList "Integration tests"
        (testFiles 
        |> List.map (fun path -> 
                        let testData = getTestData path
                        createTest (Path.GetFileName(path)) testData
                        ))

[<EntryPoint>]
let main argv =
    runTestsInAssembly defaultConfig argv
