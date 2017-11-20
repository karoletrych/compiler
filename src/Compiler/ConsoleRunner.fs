module Compiler.ConsoleRunner
open Argu
open Compiler
open System.IO
open Parser
open System.Reflection
open System.Reflection.Emit
open System
open CompilerResult
open CILGeneration

type OutputType = 
| Exe
| Library

type CLIArguments =
    | [<MainCommand; ExactlyOnce; First>] SourceFiles of path : string list
    | [<Unique>] Output of path : string
    | [<Unique>] OutputType of OutputType
    | [<Unique>] ReferencedDlls of paths : string list
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | SourceFiles _ -> "source file paths."
            | Output _ -> "output path."
            | OutputType _ -> "output type."
            | ReferencedDlls _ -> "referenced dll paths"

let writeOutputMessage errors =
    printfn "Compilation Failure"


let generateOutputFile (outputPath, outputType, referencedAssemblies) modules =
    let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(
                            AssemblyName(outputPath), AssemblyBuilderAccess.RunAndSave)
    let isExe = 
        match outputType with
        | Exe -> true
        | Library -> false
    do generateAssembly assemblyBuilder referencedAssemblies modules isExe
    do assemblyBuilder.Save(outputPath)

[<EntryPoint>]
let main argv =
    let argumentParser = ArgumentParser.Create<CLIArguments>(programName = "Compiler.exe")
    try
        let args = argumentParser.Parse()
        let referencedDlls = args.GetResult (<@ReferencedDlls@>,[])
        let outputPath = args.GetResult (<@Output@>, "Program.exe")
        let outputType = args.GetResult (<@OutputType@>, Exe)
        let sourcePaths = args.GetResult (<@SourceFiles@>, [])

        let sourceFiles =
            sourcePaths 
            |> List.map (fun name -> 
            {
                Name = (Path.GetFileNameWithoutExtension(name));
                Code = File.ReadAllText name
            })

        let referencedAssemblies =
            Assembly.GetAssembly(typeof<obj>) 
                :: (referencedDlls |> List.map Assembly.LoadFile)
        compile 
            sourceFiles
            referencedAssemblies
            |> Result.either
                (generateOutputFile 
                    (outputPath, outputType, referencedAssemblies))
                writeOutputMessage 
    with
    | :? ArguParseException as parseException -> (printfn "%s" parseException.Message)
    0
