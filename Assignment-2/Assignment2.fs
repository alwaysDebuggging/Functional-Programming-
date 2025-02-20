module Assignment2
    let rec downto1 n = 
        if n > 0 then 
            n::downto1 (n-1)
        else 
            []
        ;;
           
    let rec downto2 n = 
        match n with 
        | 0 -> []
        | n -> n :: downto2 (n-1)
        ;;

          
    let rec downto3  =
         function 
         | 0 -> []
         | n -> n :: downto3 (n-1)
         ;;
        
    
        
    let rec removeOddIdx xs =
        match xs with
        |[] -> []
        |[x] -> [x]
        |x::_::xs-> x :: removeOddIdx xs
        ;;
    

    let rec combinePair xs =
        match xs with
        |[] -> []
        |[_] -> []
        |x::n::xs-> (x,n) :: combinePair xs
        ;;
          
 
    type complex = float * float;;
  
    let mkComplex x y : complex = (x, y);;
        
    let complexToPair (a : complex) =  (fst(a),snd(a));;
    
    let (|+|)  (a,b) (c,d) : complex = (a + c, b + d) ;;
    
    let (|*|) (a,b) (c,d) : complex =
        (a * c - b * d, b * c + a * d);;
    
    let (|-|) (a,b) (c,d) : complex = (a - c, b - d);;
    
        
    //let (|/|) (a,b) (c,d) : complex = ((a/(a**2.0 + b**2.0)),(-b/(a**2.0 + b**2.0)));;
    let (|/|) (a,b) (c,d) : complex = ((a * c + b * d)/(d**2.0 + c**2.0),(b * c - a * d)/(d**2.0 + c**2.0));;

    //2.5
    let explode1 s =
        match s with
        | "" -> []
        | s -> let charArray = s.ToCharArray()
               List.ofArray(charArray)
        ;;
                   
                   
    let rec explode2 s =
        match s with
        | "" -> []
        | s -> s.[0] :: explode2 (s.Remove(0,1));;
    
    //2.6   
    let rec implode (cs: char list) =
        match cs with
        | [] -> ""
        | c::cs ->  string c + implode cs
        ;;
        
    
    let rec implodeRev (cs: char list) =
       match cs with
       | [] -> ""
       | c::cs -> implodeRev cs + string c
       ;;
    
    //2.7
    let rec toUp s =
       match s with
       |[] -> []
       |h::s -> System.Char.ToUpper h :: toUp s
       ;;
   
    let toUpper s  = s |> explode1 |> toUp |> implode ;;

    
    //2.8
    let rec ack (x,y) =
       match x, y with
       | 0, y -> y + 1 
       | x, 0 when x > 0-> ack (x - 1, 1)
       | x, y when x > 0 && y > 0 -> ack (x - 1, ack (x, y - 1))
       | _ -> failwith "todo"
       ;;
      