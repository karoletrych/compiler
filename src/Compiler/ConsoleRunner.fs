module Compiler.ConsoleRunner
open Argu
open Compiler

type CLIArguments =
    | [<MainCommand; ExactlyOnce; First>] SourceFiles of path : string list
    | [<Unique>] Output of path : string
    | [<Unique>] Stage of Stage
    | [<Unique>] Dll of bool
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | SourceFiles _ -> "source file paths."
            | Output _ -> "output path."
            | Stage _ -> "compilation stage."
            | Dll _ -> "compile to dll."

[<EntryPoint>]
let main argv =
    let argumentParser = ArgumentParser.Create<CLIArguments>(programName = "Compiler.exe")
    try
        let results = argumentParser.Parse()
        compile 
            (results.GetResult (<@SourceFiles@>, []))
            (results.GetResult (<@Output@>, "Program.exe"))
            (results.GetResult (<@Stage@>, Full))
            (results.GetResult (<@Dll@>))
    with
    | :? ArguParseException as parseException -> (printfn "%s" parseException.Message)
    0
