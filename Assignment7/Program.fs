// For more information see https://aka.ms/fsharp-console-apps

open Interpreter.Programs
open Interpreter.Eval
open Interpreter.State
open Interpreter.Language
open Interpreter.Memory
open Interpreter.StateMonad




//let test =   evalState(declare "x" >>>= setVar "x" 42 >>>= getVar "x") (mkState 0 None Map.empty)

//printfn "%A" test

(*let runProgram prog =
    42 |>
    Some |>
    mkState 10 |>
    stmntEval prog |>
    ignore
*)
// Uncomment the program you want to run

//runProgram guessANumber
//runProgram bubbleSort
