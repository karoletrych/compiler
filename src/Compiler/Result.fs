module Compiler.Result

type Failure<'T> = 
| ParsingError of 'T
| CannotResolveType of 'T

type Result<'TSuccess, 'TError> = 
| Success of 'TSuccess * Failure<'TError> list
| Failure of Failure<'TError> list

let succeed x = Success (x, []) 
let failure error = Failure [error]
let succeedUnit = Success ((), []) 

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

let addResults r1 r2 = 
     match (r1, r2) with
     | Success (s1, errors1), Success (s2, errors2) -> Success (s1, errors1 @ errors2)
     | Failure f1, Success _  -> Failure f1
     | Success _ , Failure f2 -> Failure f2
     | Failure f1, Failure f2 -> Failure (f1 @ f2)

let (&&=) v1 v2 = 
    addResults v1 v2
let (>>=) twoTrackInput switchFunction =
    bind switchFunction twoTrackInput
