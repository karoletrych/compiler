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
| Dll

type CLIArguments =
    | [<MainCommand; First>] SourceFiles of path : string list
    | [<Unique>] [<AltCommandLine("-o")>] Output of path : string
    | [<Unique>] [<AltCommandLine("-O")>] OutputType of OutputType
    | [<Unique>] [<AltCommandLine("-R")>] ReferencedDlls of paths : string list
    | [<Unique>] [<AltCommandLine("-S")>] PrintIR
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | SourceFiles _ -> "source file paths."
            | Output _ -> "output path."
            | OutputType _ -> "output type."
            | ReferencedDlls _ -> "referenced dll paths"
            | PrintIR _ -> "print intermediate representation"

let writeOutputMessage errors =
    printfn "Compilation Failure: " 
    printfn "%s" (toString errors)

let baseDirectory =
    Directory.GetParent(Assembly.GetExecutingAssembly().Location).FullName + string Path.DirectorySeparatorChar

let makeRelativePath path= 
    path
    |>Uri 
    |> (fun file -> 
                    Uri.UnescapeDataString(
                        (Uri baseDirectory).MakeRelativeUri(file)
                            .ToString()
                            .Replace('/', Path.DirectorySeparatorChar))
    )
let getInputFiles () =
    printfn "No input files specified. Searching for .ifr files:"
    printfn "Base directory: %s" baseDirectory
    let files = 
        Directory.GetFiles(baseDirectory, "*.ifr", SearchOption.AllDirectories)
        |> Array.map makeRelativePath
        |> Array.toList
    files |> List.map (fun f -> printfn "%s" f) |> ignore
    files 
    

let generateOutputFile (outputPath, isExe, referencedAssemblies) modules =
    let assemblyBuilder = 
        AppDomain.CurrentDomain.DefineDynamicAssembly(
            AssemblyName(Path.GetFileNameWithoutExtension(outputPath)), AssemblyBuilderAccess.RunAndSave)
    
 
    do generateAssembly assemblyBuilder referencedAssemblies modules isExe
    do assemblyBuilder.Save(Path.GetFileName(outputPath))
    printfn "Writing output file to: %s" outputPath
    do File.Move(Path.GetFileName(outputPath), outputPath)

let validateFilePaths filePaths =
    filePaths
    |> List.iter (fun p -> if not (File.Exists(p)) then failwithf "File %s does not exist" p)


[<EntryPoint>]
let main argv =
    let argumentParser = ArgumentParser.Create<CLIArguments>(programName = "Compiler.exe")
    try
        let args = argumentParser.Parse()
        let referencedDlls = args.GetResult (<@ReferencedDlls@>,[])
        let isExe = 
            match args.GetResult (<@OutputType@>, Exe) with
            | Exe -> true
            | Dll -> false
        let outputPath = 
            if args.Contains(<@Output@>)
            then args.GetResult(<@Output@>)
            else match isExe with
                 | true -> "Program.exe"
                 | false -> "Library.dll"
        
        let printIR = args.Contains <@PrintIR@>
        let sourcePaths = 
            args.GetResult (<@SourceFiles@>, [])
            |> function
               | [] ->  getInputFiles ()
               | paths -> validateFilePaths paths; paths
        let sourceFiles =
            sourcePaths 
            |> List.map (fun filePath -> 
            {
                Path = Path.ChangeExtension(filePath, null)
                Code = File.ReadAllText filePath
            })

        let referencedAssemblies =
            Assembly.GetAssembly(typeof<obj>) 
                :: (referencedDlls |> List.map Assembly.LoadFile)
        compile 
            sourceFiles
            referencedAssemblies
            isExe
            |> Result.either
                (
                    fun modules -> 
                        if printIR then File.WriteAllText(Path.Combine(Environment.CurrentDirectory, "ir.fsx"), (sprintf "%A" modules))
                        generateOutputFile (outputPath, isExe, referencedAssemblies) modules
                )
                writeOutputMessage 
    with
    | :? ArguParseException as parseException -> (printfn "%s" parseException.Message)
    | exc -> printfn "%s" (exc.ToString())
    0
