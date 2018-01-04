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
let exePath = Path.Combine(Assembly.GetEntryAssembly().Location, @"..\")
let outputPath = Path.Combine(exePath, "a.exe")
let testDirectory = Path.Combine(exePath, @"..\..\..\tests")
let files = 
    System.IO.Directory.GetFiles(testDirectory)
    |> List.ofArray
let testFiles = 
    files 
    |> List.where (fun f -> Path.GetExtension(f) = ".ifr")

let resultFiles = 
    files 
    |> List.filter (fun f -> Path.GetExtension(f) = ".out") 
let withCorrespondingOutput input = 
    input, resultFiles |> List.find (fun f -> 
                        Path.GetFileNameWithoutExtension(f) = Path.GetFileNameWithoutExtension(input))

let readFiles (input, output) = ({Path = input; Code = File.ReadAllText input}, File.ReadAllText output)
let mscorlib = [Assembly.GetAssembly(typeof<obj>)]
let compile input = compile [input] mscorlib true
let generateAssembly (irResult : CompilerResult<IR.Module list>)=
    let ir = irResult.Value
    File.WriteAllText(Path.Combine(exePath, "ir.fsx"),(sprintf "%A" ir))

    let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(
                            AssemblyName("a"), AssemblyBuilderAccess.RunAndSave)
    generateAssembly assemblyBuilder mscorlib ir true
    assemblyBuilder

let execute (assemblyBuilder : AssemblyBuilder) =
    use stringWriter = new StringWriter()
    let originalOut = Console.Out
    Console.SetOut(stringWriter)

    if File.Exists(outputPath) then File.Delete(outputPath)
    assemblyBuilder.Save("a.exe")
    try
        assemblyBuilder.EntryPoint.Invoke(null, Array.empty) |> ignore
    with 
        | ex -> printfn "%s" ex.Message
    
    Console.SetOut(originalOut)
    stringWriter.ToString()

let getTestData = 
    withCorrespondingOutput 
    >> readFiles
    >> fun (input, expectedOutput) -> 
            (fun () ->
                input 

                |> compile
                |> generateAssembly 
                |> execute),          expectedOutput

let createTest testName testData = 
    let (test, expectedOutput) = testData
    if testName = "enumerateDictionary.ifr"
    then ftestCase testName (fun _ -> 
        let output = test()
        Expect.equal output expectedOutput testName)
    else testCase testName (fun _ -> 
        let output = test()
        Expect.equal output expectedOutput testName)

    // let output = test()
    // testCase testName (fun _ -> Expect.equal output expectedOutput testName)

let allTests = 
    (testFiles 
    |> List.map (fun path -> 
                let testData = getTestData path
                createTest (Path.GetFileName(path)) testData
                ))

[<Tests>]
let tests = 
    testSequenced <| testList "Integration tests"
        allTests

[<EntryPoint>]
let main argv =
    runTestsInAssembly defaultConfig argv

