module Compiler.Runner

open System.IO
open Argu
open Compiler.CompilerResult
open Compiler.Parser
open Compiler.TypeFinder
open Compiler.TypeInference

type Stage =
    | SyntaxCheck
    | TypeFinding
    | TypeInference
    | SemanticCheck 
    | IRGeneration
    | CILGeneration
    | Full

type CLIArguments =
    | [<MainCommand; ExactlyOnce; First>] SourceFiles of path : string list
    | [<Unique>] Output of path : string
    | [<Unique>] Stage of Stage
    | [<Unique>] PrintSyntax
    | [<Unique>] PrintInference
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | SourceFiles _ -> "source file paths."
            | Output _ -> "output path."
            | Stage _ -> "compilation stage."
            | PrintSyntax _ -> "print syntax tree."
            | PrintInference _ -> "print inferred expression types."

let compile 
    (sourceFilePaths : string list)
    (outputPath : string)
    (stage : Stage)
    (printSyntax : bool)
    (printInference : bool) =
    let parse sourceCode = 
        Parser.parse sourceCode
                     |> (fun parsed ->   
                         if printSyntax 
                         then printfn "%A" parsed 
                         parsed)
    let parseFile path = parse (File.ReadAllText path)

    match stage with
    | SyntaxCheck -> sourceFilePaths 
                        |> List.map parseFile
                        |> ignore
    | TypeFinding ->
                    let buildModule (declarationsResult, name) = 
                        declarationsResult |> CompilerResult.map (fun decls -> Ast.Module.create name decls)
                    let modulesResult =
                        sourceFilePaths 
                        |> List.zip (sourceFilePaths |> List.map parseFile)
                        |> List.map buildModule
                        |> List.reduce (CompilerResult.map2 (+))
                    ()
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
            (results.Contains <@PrintSyntax@>)
            (results.Contains <@PrintInference@>)
    with
    | :? ArguParseException as parseException -> (printfn "%s" parseException.Message)
    0
