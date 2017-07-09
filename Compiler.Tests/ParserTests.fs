module Tests

open Expecto
open Compiler.Parser

[<Tests>]
let tests =
  testList "parser" [
    testCase "parser" <| fun _ ->
      Expect.equal "a" (Compiler.Parser.parse "a") "tests work"
]