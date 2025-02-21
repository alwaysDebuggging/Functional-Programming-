module Interpreter.State

    open Result
    open Language
    
    let validVariableName (v: string) =
         
         let b = (v.Chars(0) |> System.Char.IsAsciiLetter ) || v.Chars(0) = '_'
         let predicate c  = c |> System.Char.IsAsciiLetterOrDigit || c = '_'
         let a = String.forall predicate v 
         
         b && a
         
        ;;
        
 
    let reservedVariableName v =
         let lst = ["if"; "then"; "else"; "while"; "declare"; "print"; "random"; "fork"; "__result__"]
         //List.contains  v lst
         
         List.exists ((=)v) ["if"; "then"; "else"; "while"; "declare"; "print"; "random"; "fork"; "__result__"]
       
    ;;
        
    type state = {
        map: Map<string, int>
        
    }
    
    let mkState () = {map= Map.empty}
           
    let declare (x: string) (st: state) =
        if not (reservedVariableName x) && validVariableName(x) && not (st.map.ContainsKey x) then
           Some  {
             map = st.map.Add(x,0) // returned a new Map containing the old and the new value
                                   // new state containing a new Map with the old values and the new value
             
             }
            
        else
            None
        ;;
        
    let getVar _ = failwith "not implemented"
    let setVar _ = failwith "not implemented"
    
    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"     