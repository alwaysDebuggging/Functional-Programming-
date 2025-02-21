module Interpreter.Eval

    open Interpreter.Language
    open Result
    open Language
    open State
    
    let rec arithEval (q: aexpr) (st: state) =
        match q with
        | Num int -> Some int
        | Add(ex1, ex2) -> Option.bind (fun x ->
                               Option.bind (fun y -> Some (x + y)) (arithEval ex2 st)
                           ) (arithEval ex1 st)
                           
        | Mul (ex1, ex2) -> let a = arithEval ex1 st
                            let b = arithEval ex2 st
                            
                            Option.bind(fun x ->
                                Option.bind(fun y-> Some x * y
                                    ) b
                                ) a
        | Div (ex1, ex2) -> None
        
        | Mod (ex1, ex2) -> None
        ;;
        
        
        
    let boolEval _ = failwith "not implemented"
    let stmntEval _ = failwith "not implemented"