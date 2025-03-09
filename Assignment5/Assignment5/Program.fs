module Interpreter.Program
open Interpreter.Language
open Interpreter.Eval
open Interpreter.Memory
open Interpreter.State


let [<EntryPoint>] main _ =
    
    let x = 10 |> mkState |> stmntEval (Seq (Declare "x", Seq(Alloc ("x", Num 3), Seq(Declare "result", Assign("result", MemRead (Var "x")))))) |> Option.bind (getVar "result")
    
    
    printfn "%A" x
    0
