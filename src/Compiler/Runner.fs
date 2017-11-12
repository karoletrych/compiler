module Compiler.Runner

open System.Reflection
open System.IO
open Argu
open CompilerResult
open Parser
open TypeFinding
open TypeResolving
open TypeInference
open ReferencedAssembliesMetadata
open Compiler.TypeIdentifiersFinding

type Stage =
    | SyntaxCheck
    | TypeFinding
    | TypeResolving
    | TypeInference
    | SemanticCheck 
    | IRGeneration
    | CILGeneration
    | Full

type CLIArguments =
    | [<MainCommand; ExactlyOnce; First>] SourceFiles of path : string list
    | [<Unique>] Output of path : string
    | [<Unique>] Stage of Stage
    | [<Unique>] PrintSyntaxTree
    | [<Unique>] PrintTypeInference
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | SourceFiles _ -> "source file paths."
            | Output _ -> "output path."
            | Stage _ -> "compilation stage."
            | PrintSyntaxTree _ -> "print syntax tree."
            | PrintTypeInference _ -> "print inferred expression types."

let compile 
    (sourceFilePaths : string list)
    (outputPath : string)
    (stage : Stage)
    (printSyntax : bool)
    (printInference : bool) =
    let modulesWithPaths = 
        sourceFilePaths 
        |> List.zip (sourceFilePaths |> List.map File.ReadAllText)
    let externalTypes = externalTypes [Assembly.GetAssembly(typeof<obj>)]
    
    match stage with
    | SyntaxCheck -> 
        modulesWithPaths 
        |> parseModules
        |> ignore
    | TypeResolving -> 
        modulesWithPaths 
        |> parseModules
        >>= allKnownTypeIdentifiers externalTypes
        >>= resolve
        |> ignore  
    // | TypeInference ->
    //     modulesWithPaths 
    //     |> parseModules
    //     >>= allKnownTypeIdentifiers externalTypes
    //     >>= resolve
    //     >>= inferTypes 
    //     |> ignore  
    | _ -> printfn "Stage not implemented."
    ()

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "Compiler.exe")
    try
        let results = parser.Parse()
        compile 
            (results.GetResult (<@SourceFiles@>, []))
            (results.GetResult (<@Output@>, "Program.exe"))
            (results.GetResult (<@Stage@>, Full))
            (results.Contains <@PrintSyntaxTree@>)
            (results.Contains <@PrintTypeInference@>)
    with
    | :? ArguParseException as parseException -> (printfn "%s" parseException.Message)
    0
