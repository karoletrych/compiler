module Tests

open Expecto
open Compiler.Parser

[<Tests>]
let tests =
  testList "samples" [
    testCase "universe exists" <| fun _ ->
      let subject = true
      Expect.isTrue subject "I compute, therefore I am."

    testCase "should fail" <| fun _ ->
      let subject = false
      Expect.isTrue subject "I should fail because the subject is false."

    testCase "parser" <| fun _ ->
    Expect.equal "a" (Compiler.Parser.parse "a") "tests work"
  ]