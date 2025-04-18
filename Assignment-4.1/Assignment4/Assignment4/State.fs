module Interpreter.State
    open Language
    open Result
    
    
    let validVariableName (v: string) =
         let b = (v.Chars(0) |> System.Char.IsAsciiLetter ) || v.Chars(0) = '_'
         let predicate c  = c |> System.Char.IsAsciiLetterOrDigit || c = '_'
         let a = String.forall predicate v 
         
         b && a
    ;;
    let reservedVariableName v =
         let lst = ["if"; "then"; "else"; "while"; "declare"; "print"; "random"; "fork"; "__result__"]
         List.contains  v lst
    ;;   
    type state = {  
        map: Map<string, int>   
    }
    ;;
    let mkState () = {map= Map.empty};;      
    let declare (x: string) (st: state) : Result<state, error> =
        if not (reservedVariableName x) && validVariableName(x) && not (st.map.ContainsKey x) then
           Ok {
             map = st.map.Add(x,0) // returned a new Map containing the old and the new value
                                   // new state containing a new Map with the old values and the new value
             }
        else
            Error (VarNotDeclared x) 
            Error (ReservedName x) 
            Error (InvalidVarName x) 
    ;;      
    let getVar (x: string) (st: state) : Result<int, error> =
        if st.map.ContainsKey x then
           Ok (st.map.TryFind x)
        else
            Error (VarNotDeclared x)
            
    ;;  
    let setVar (x: string) (v: int) (st: state) : Result<state, error> =
        if  st.map.ContainsKey x then
            Ok {
             map = st.map.Add(x,v) // returned a new Map containing the old and the new value
                                   // new state containing a new Map with the old values and the new value
             }
        else
            Error (VarNotDeclared x)
    ;;    