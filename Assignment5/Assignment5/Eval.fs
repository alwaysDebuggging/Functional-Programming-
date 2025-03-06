module Interpreter.Eval

    open Interpreter.Language
    open Interpreter.Memory
    open Result
    open Language
    open Interpreter.State
       
    let rec arithEval (a: aexpr) (st: state) =
        match a with
        | Num int -> Some int
        
        | Var v ->  getVar v st
            
        | Add(ex1, ex2) ->  let a = arithEval ex1 st
                            let b = arithEval ex2 st
                            
                            Option.bind (fun x ->
                                Option.bind (fun y -> Some (x + y)
                                ) b
                            ) a
                            
        | Mul (ex1, ex2) -> let a = arithEval ex1 st
                            let b = arithEval ex2 st
                            
                            Option.bind(fun x ->
                                Option.bind(fun y-> Some (x * y)
                                ) b
                            ) a
        | Div (ex1, ex2) -> let a = arithEval ex1 st
                            let b = arithEval ex2 st
                            
                            Option.bind(fun x ->
                                Option.bind(fun y -> if y <> 0 then Some (x / y) else None
                                ) b
                            ) a
        | Mod (ex1, ex2) -> let a = arithEval ex1 st
                            let b = arithEval ex2 st
                            
                            Option.bind(fun x ->
                                Option.bind(fun y-> if y <> 0 then Some (x % y) else None 
                                )b
                            )a
        | MemRead e1  ->  Option.bind (fun y -> getMem y st )  (arithEval e1 st)
    ;;
    
    let rec boolEval b st : bool option =
        match b with
        | TT ->  Some true
        | Eq (ex1, ex2) -> let a = arithEval ex1 st
                           let b = arithEval ex2 st
                           
                           Option.bind (fun x ->
                               Option.bind(fun y ->  Some ((=) x y)
                               ) b
                           )a
                        
        | Lt (ex1, ex2) -> let a = arithEval ex1 st 
                           let b = arithEval ex2 st
                           
                           Option.bind (fun x ->
                               Option.bind(fun y ->  Some ((<) x y)
                               ) b
                           )a

        | Conj (ex1, ex2) -> let a = boolEval ex1 st 
                             let b = boolEval ex2 st
                           
                             Option.bind (fun x ->
                                Option.bind(fun y ->  Some ((&&) x y)
                                ) b //(boolEval ex2 st)
                             ) a //(boolEval ex1 st)
                          
                             //Some ((&&) a b)
        
        | Not ex1 -> let a = boolEval ex1 st
                     
                     a |> Option.bind(fun x -> Some (not x))
    ;;
 
    let rec stmntEval (s: stmnt) (st: state) : state option =
        match s with
        | Skip -> Some st 
        | Declare v -> declare v st 
        | Assign(v, a) -> let b = arithEval a st
                          match b with
                          | None -> None
                          | Some x -> setVar v x st
                          
        | Seq(s1, s2) -> let b = stmntEval s1 st
                         match b with
                         | None -> None
                         | Some stt -> stmntEval s2 stt 

        | If(gaurd, s1, s2) -> let b = boolEval gaurd st
                               match b with
                               | None -> None
                               | Some x -> if x = true then stmntEval s1 st else  stmntEval s2 st

        | While(guard, s) -> let b = boolEval guard st
                             match b with
                             | None -> None
                             | Some x -> if x = true then
                                            let c = stmntEval s st
                                            match c with
                                            | None -> None
                                            | Some stt -> stmntEval (While(guard, s)) stt 
                                         else
                                             Some st
        | Alloc (x, e) -> Option.bind ()()
            
            
        | MemWrite (e1, e2) -> Some st
        | Free (e1, e2) -> Some st                        
    ;;
        