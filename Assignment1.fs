module Assignment1

    open System
    

    let sqr x = x * x;; 

   
    let rec pow x n  = 
            match x, n  with
            | x, n -> System.Math.Pow(x, n);;

  
    let rec fib n =
        match n with   
        | 0 -> 0
        | 1 -> 1
        | n -> fib(n-1) + fib(n-2);; 


 
    let rec sum n =
        match n with  
        | 0 -> 0
        | n -> n + sum(n-1);;


    let dup s =
         s + "" + s;;


    let rec dupn s n = 
        match s, n with 
        |s, 0 -> ""
        |s, n -> s + dupn s (n-1);;


    let readFromConsole () = System.Console.ReadLine().Trim();;
    let tryParseInt (str : string) = System.Int32.TryParse str;;


    let rec readInt() =
        let consoleInput = readFromConsole()
        let tryParseInput = tryParseInt (consoleInput)
        let (x,y) = tryParseInput
        if x = false then 
            printfn "%s is not an integer" consoleInput 
            readInt()
        else 
           y;;
        
       
    let timediff (x,y) (n,p) =
       let T1Tuple = x * 60 + y  
       let T2Tuple = n * 60 + p 
       T2Tuple - T1Tuple;;
     

    let minutes (x,m) = 
        timediff (00,00) (x,m);;


    let rec bin (n,k) =
        match n, k with 
        |n, 0-> 1
        |n, k when n = k -> 1
        |n, k -> bin (n - 1, k - 1) + bin (n - 1,  k);;
  
  
    let curry f x y = f (x, y);;
    

    let uncurry f (x,y) = f x y;;