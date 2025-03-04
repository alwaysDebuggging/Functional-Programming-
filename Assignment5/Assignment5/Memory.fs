module Interpreter.Memory
    
    type memory = {
        map: Map<int, int>
        next: int
    };;

    let empty (x: int)  (mem : memory) =
        {
            map = Map.empty
            next = 0 
        };;
    let rec alloc (size: int) (mem: memory) : (memory * int) option  =
        let rec aux index cDict =
            if index < size + mem.next then
                Map.add index 0 cDict
            else
                cDict
           
        if size > 0 then
            Some ({
               map =  aux mem.next mem.map
               next = size + mem.next 
            }, mem.next)
        else 
            None
        
            
            
            
            
        if size > 0 then
            Some (mem, mem.next + size - 1)
        else None
    ;;
        
    
    let free _ = failwith "not implemented"
        
    let setMem _ = failwith "not implemented"
        
    let getMem _ = failwith "not implemented"