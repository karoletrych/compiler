namespace Compiler.CompilerResult
open Compiler.Ast


type Failure = 
| ParsingError of string
| CannotResolveType of TypeSpec

type CompilerResult<'TSuccess> = 
| Success of 'TSuccess * Failure list
| Failure of Failure list

module Result = 
    let succeed x = Success (x, []) 
    let failure error = Failure [error]
    let succeedUnit = Success ((), []) 

    let get = function
        | Success (x,errors) -> (x,errors)
        | Failure errors -> failwith "get called on CompilerResult being Failure" 

    let either fSuccess fFailure = function
        | Success (x,errors) -> fSuccess (x,errors) 
        | Failure errors -> fFailure errors 

    let mergeErrors errors result =
        let fSuccess (x,successErrors) = 
            Success (x, errors @ successErrors) 
        let fFailure failureErrors = 
            Failure (failureErrors @ errors) 
        either fSuccess fFailure result

    let bind f result =
        let fSuccess (x,errors) = 
            f x |> mergeErrors errors
        let fFailure errs = 
            Failure errs 
        result |> either fSuccess fFailure 

    let map f result =
        let fSuccess (x, errors) = 
            f x |> (fun x -> Success(x, errors)) 
        let fFailure errs = 
            Failure errs 
        result |> either fSuccess fFailure 

    let map2 op r1 r2 = 
         match (r1, r2) with
         | Success (s1, errors1), Success (s2, errors2) -> Success (op s1 s2, errors1 @ errors2)
         | Failure f1, Success _  -> Failure f1
         | Success _ , Failure f2 -> Failure f2
         | Failure f1, Failure f2 -> Failure (f1 @ f2)


    let (&&=) v1 v2 = 
        map2 v1 v2
    let (>>=) twoTrackInput switchFunction =
        bind switchFunction twoTrackInput
