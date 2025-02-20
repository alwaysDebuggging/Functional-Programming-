module Interpreter.Eval

    open Interpreter.Language
    open Result
    open Language
    open State
    
    let rec arithEval (a: aexpr) (st: state) =
        match a with
        | Num int -> Num int
        | Add(ex1, ex2) -> 
        | Mul (ex1, ex2) ->
        | Div (ex1, ex2) ->
        | Mod (ex1, ex2) -> 
        ;;
        
        
        
    let boolEval _ = failwith "not implemented"
    let stmntEval _ = failwith "not implemented"