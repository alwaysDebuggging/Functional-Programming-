module Interpreter.State
    open System
    open Memory
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
         List.contains  v lst
    ;;
    
    type state = {
        map: Map<string, int>
        memory: Memory.memory
        rng: System.Random
        prog: program
    }
    ;;
    
    let mkState (memSize: int) (oseed: int option) (prog: program) : state =
        {
            map = Map.empty
            memory =  Memory.empty memSize
            rng = match oseed with
                           | Some x -> System.Random(x)
                           | None -> System.Random()
            prog = prog
        }
    ;;
    
    let random (st: state) : int =
        st.rng.Next()
    ;;
        

    let declare (x: string) (st: state) =
        if not (reservedVariableName x) && validVariableName(x) && not (st.map.ContainsKey x) then
           Some  {
             st with map = st.map.Add(x,0) // returned a new Map containing the old and the new value
                                           // new state containing a new Map with the old values and the new value
             }
        else
            None
    ;;      
    let getVar (x: string) (st: state) =
        st.map.TryFind x
    ;;  
    let setVar (x: string) (v: int) (st: state)  =
        if  st.map.ContainsKey x then
            Some  {
             st with map = st.map.Add(x,v) // returned a new Map containing the old and the new value
                                           // new state containing a new Map with the old values and the new value
             }
        else
            None
    ;;
    
    let alloc (x: string) (size: int) (st: state) : state option =
        // match Memory.alloc size st.memory with
        // | Some (mem', oldNext) ->
        //     let st' = {st with memory = mem'}
        //     
        //     setVar x oldNext st'
        //                             
        // | _ -> None
        //
        
        Memory.alloc size st.memory |> Option.bind(fun (mem', oldNext) -> setVar x oldNext {st with memory = mem'})
        
        
    let free (ptr: int) (size: int) (st: state) : state option =
        Memory.free ptr size st.memory |> Option.bind(fun x -> Some {
            st with memory =  x
             }
          )
    ;;
    let getMem (ptr: int) (st: state) : int option =
        Memory.getMem ptr st.memory
    ;;
    let setMem  (ptr: int) (v: int) (st: state): state option =
        match Memory.setMem ptr v st.memory with
        | Some x ->  Some {
            st with memory = x
            }
        | _ -> None
    ;;
        
    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"     
    
    let pushFrame _ = failwith "not implementd"
    let popFrame _ = failwith "not implemented"