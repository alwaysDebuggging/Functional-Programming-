module Interpreter.Memory

    //open Interpreter.State
    
    type memory = {
        map: Map<int, int>
        next: int
    };;

    let empty (x: int) =
        {
            map = Map.empty
            next = 0 
        }
    ;;
    
    let alloc (size: int) (mem: memory) : (memory * int) option   =
        let rec aux currentIndex currentMemoryMap =
            if currentIndex < size + mem.next then
                aux (currentIndex + 1)  (Map.add currentIndex 0 currentMemoryMap)
            else
                currentMemoryMap
        if size > 0 then
            Some ({
               map =  aux mem.next mem.map
               next = size + mem.next 
            }, mem.next)
        else 
            None
    ;;
        
    
    let free (ptr: int) (size: int) (mem: memory) : memory option =
        let rec freeUpSpace currentIndex  spaceSize currentMemoryMap =
            if spaceSize > 0 then
                if Map.containsKey currentIndex currentMemoryMap then 
                    freeUpSpace (currentIndex + 1 ) (spaceSize - 1) (Map.remove currentIndex currentMemoryMap)
                else
                    None
            else
                Some currentMemoryMap
                
        if size > 0 then
            freeUpSpace ptr size mem.map |> Option.bind(fun d ->
                Some {
                   map = d
                   next = mem.next
                }
            ) 
        else
           
           None
    ;;
        
    let setMem (ptr: int) (v: int) (mem: memory): memory option  =
        if mem.map.ContainsKey ptr then
            Some {
                map = mem.map.Add (ptr, v)
                next = mem.next
            }
        else
            None
    ;;
        
    let getMem (ptr: int) (mem: memory) : int option =
        mem.map.TryFind ptr
    ;;