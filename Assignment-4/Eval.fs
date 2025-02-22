module Interpreter.Eval

    open Interpreter.Language
    open Result
    open Language
    open State
    
    let rec arithEval (a: aexpr) (st: state) =
        match a with
        | Num int -> Some int
        
        | Var v -> st.map.TryFind v 
                   
            
        | Add(ex1, ex2) ->  match (arithEval ex1 st , arithEval ex2 st) with
                                | (Some exp1, Some exp2) -> Some (exp1 + exp2)
                                | _ -> failwith "todo"
                     
        | Mul (ex1, ex2) -> match (arithEval ex1 st , arithEval ex2 st) with
                                | (Some exp1, Some exp2) -> Some (exp1 * exp2)
                                | _ -> failwith "todo"
                            
        | Div (ex1, ex2) -> match (arithEval ex1 st , arithEval ex2 st) with
                                | (Some exp1, Some exp2) -> if ex2 <> Num 0 then
                                                                Some (exp1 / exp2)
                                                            else
                                                                None
                                | _ -> failwith "todo"
                                
 
        | Mod (ex1, ex2) -> match (arithEval ex1 st , arithEval ex2 st) with
                                | (Some exp1, Some exp2) -> if ex2 <> Num 0 then
                                                                Some (exp1 % exp2)
                                                            else
                                                                None
                                | _ -> failwith "todo"
    ;;
        
        
    let rec arithEval2 (a: aexpr) (st: state) =
        match a with
        | Num int -> Some int
        
        | Var v ->  st.map.TryFind v 
            
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
 
                                // match (arithEval ex1 st , arithEval ex2 st) with
                                // | (Some exp1, Some exp2) when exp2 <> 0 -> Some (exp1 / exp2)
                                // | _ -> failwith 
                                
        
        | Mod (ex1, ex2) -> let a = arithEval ex1 st
                            let b = arithEval ex2 st
                            
                            Option.bind(fun x ->
                                Option.bind(fun y-> if y <> 0 then Some (x % y) else None
                                ) b
                            ) a
    ;;
    
    let boolEval _= failwith "not implemented"
    // decleared should call declare with v and a state st since it says o return st
    // with variable v decleared
        
        
        
    let stmntEval _= failwith "not implemented"
        