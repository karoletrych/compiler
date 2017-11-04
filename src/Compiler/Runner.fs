module Compiler.Runner

open System.IO
open Argu
open Compiler.CompilerResult
open Compiler.Parser
open Compiler.TypeFinding
open Compiler.TypeInference
open Compiler.Ast

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
    let parse sourceCode = 
        Parser.parseDeclarations sourceCode
                     |> (fun parsed ->   
                         if printSyntax 
                         then printfn "%A" parsed 
                         parsed)
    let parseFile path = parse (File.ReadAllText path)

    let syntaxCheck = 
        sourceFilePaths 
        |> List.map parseFile
    let findTypes = 
        sourceFilePaths 
        |> List.zip (sourceFilePaths |> List.map File.ReadAllText)
        |> Parser.createModule
        |> Result.bind TypeFinding.allKnownTypes 
    let resolveTypes = findTypes |> Result.bind 
    let inferTypes = resolveTypes |> Result.bind inferTypes 

    match stage with
    | SyntaxCheck -> syntaxCheck |> ignore
    | TypeFinding -> findTypes |> ignore                    
    | TypeResolving -> resolveTypes |> ignore
    | TypeInference -> inferTypes |> ignore
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
