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
                                | _ -> None
                     
        | Mul (ex1, ex2) -> match (arithEval ex1 st , arithEval ex2 st) with
                                | (Some exp1, Some exp2) -> Some (exp1 * exp2)
                                | _ -> None
                            
        | Div (ex1, ex2) -> match (arithEval ex1 st , arithEval ex2 st) with
                                | (Some _, Some 0) -> None
                                | (Some exp1, Some exp2)-> Some (exp1 / exp2)       
                                | _ -> None
                            
                            
                            // let a = arithEval ex1 st
                            // let b = arithEval ex2 st
                            //
                            // match (a, b) with
                            // | (Some _, Some 0) -> None
                            // | (Some exp1, Some exp2)-> Some (exp1 / exp2)       
                            // | _ -> None
                                
                                // match (arithEval ex1 st , arithEval ex2 st) with
                                // | (Some exp1, Some exp2) -> if ex2 <> Num 0 then
                                //                                 Some (exp1 / exp2)
                                //                             else
                                //                                 None
                                // | _ -> None
                                
 
        | Mod (ex1, ex2) -> match (arithEval ex1 st , arithEval ex2 st) with
                                | (Some _, Some 0) -> None
                                | (Some exp1, Some exp2)-> Some (exp1 / exp2)       
                                | _ -> None
                            
                            
                            
                            // let a = arithEval ex1 st
                            // let b = arithEval ex2 st
                            //
                            // match (a, b) with
                            // | (Some _, Some 0) -> None
                            // | (Some exp1, Some exp2)-> Some (exp1 % exp2)                  
                            // | _ -> None
                            
                            
                            
                                // match (arithEval ex1 st , arithEval ex2 st) with
                                // | (Some exp1, Some exp2) -> if ex2 <> Num 0 then
                                //                                 Some (exp1 % exp2)
                                //                             else
                                //                                 None
                                // | _ -> None
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
                                )b
                            )a
                              // second way to do it
                            
                            ///Option.bind(fun x -> Option.bind(fun y-> if y <> 0 then Some (x % y) else None ) (arithEval2 ex2 st)) (arithEval2 ex1 st)           
    ;;
    
    let rec boolEval b st : bool option =
        match b with
        | TT ->  Some true
        | Eq (ex1, ex2) -> let a = arithEval2 ex1 st
                           let b = arithEval2 ex2 st
                           
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

        | Conj (ex1, ex2) -> (*let a = boolEval ex1 st 
                             let b = boolEval ex2 st*)
                             
                             
                             //Option.bind(fun  x y ->  Some ((&&) x y)) a b
                             
                             Option.bind (fun x ->
                                Option.bind(fun y ->  Some ((&&) x y)
                                ) (boolEval ex2 st)
                             ) (boolEval ex1 st)
                          
                             //Some ((&&) a b)
        
        | Not ex1 -> let a = boolEval ex1 st
                     
                     a |> Option.bind(fun x -> Some (not x))
                     
        
    // decleared should call declare with v and a state st since it says o return st
    // with variable v decleared
        
        
        
    let stmntEval _= failwith "not implemented"
        