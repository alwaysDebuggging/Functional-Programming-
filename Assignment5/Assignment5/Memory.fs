module Interpreter.Memory

    open Interpreter.State
    
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
    
    let alloc (size: int) (mem: memory)   =
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
        
    
    let free (ptr: int) (size: int) (mem: memory)  =
        let rec freeUpSpace currentIndex sizeToFreeUp currentMemoryMap =
            if currentIndex < sizeToFreeUp + size - 1 then
                freeUpSpace (currentIndex + 1 ) (sizeToFreeUp - 1) (Map.remove currentIndex currentMemoryMap)
            else
                currentMemoryMap
                
        if size > 0 then
            Some ({
               map = freeUpSpace ptr size mem.map
               next = mem.next
            },mem.next)
        else
           
           None
    ;;
        
    let setMem (ptr: int) (v: int) (mem: memory) =
        if mem.map.ContainsKey v then
            Some {
                map = mem.map.Add (ptr, v)
                next = mem.next
            }
        else
            None
    ;;
        
    let getMem (ptr: int) (mem: memory) =
        if mem.map.ContainsKey ptr then
            mem.map.TryFind ptr
        else
            None
    
    ;;