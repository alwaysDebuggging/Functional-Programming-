module Assignment3

    open Microsoft.Win32
    
    // Question 3.1
    let add5 x = x + 5;; 
    let mul3 x = x * 3

    let add5mul3 x = x |> add5 |> mul3;; 
    let add5mul3_2 = add5 >> mul3;;
    

    // Question 3.2
    let add5_2 f x = f x + 5 ;;
    let mul3_2 f x = f x * 3;;
    
    // Question 3.3
    let rec downto4 (f: int ->'a ->'a) (n: int ) (e: 'a) =
        match n with
        | n when n <= 0 -> e
        | n -> downto4 f (n-1) (f n e)
        ;;
        
                
    let fac (x : int) = 
        downto4(fun x n -> x * n) x 1 

    
    let range (g: int -> 'a) (x: int ) =
        downto4 (fun (y: int ) (acc: list<_>) -> g y :: acc ) x []

    // Question 3.4
    
    let rec double (x : int list) =
        match x with
        | [] -> []
        | x :: xs -> x * 2 :: double xs
        
    let double_2 x = List.map(fun x -> x * 2) x 

    
    // Question 3.5
    
    let rec stringLength (s: string list) = 
        match s with 
        | [] -> [] 
        | s :: ss -> s.Length :: stringLength ss
        
    let stringLength_2 x = List.map(fun (x : string) -> x.Length) x
    

    // Question 3.6
    
    let rec keepEven (e: int list) =
        match e with
        | [] -> []
        | e :: ex  when e % 2 = 0 -> e :: keepEven ex
        | _ :: ex -> keepEven ex ;;
        
                       
    let keepEven_2 e =
        List.filter(fun x -> x % 2 = 0) e
    
    
    // Question 3.7
    let rec keepLengthGT5 (x : string list) =
        match x with
        | [] -> []
        | x :: xs when x.Length > 5 -> x :: keepLengthGT5 xs
        | _ :: xs -> keepLengthGT5 xs;;
        
    let keepLengthGT5_2 x= List.filter(fun (x:string) -> x.Length > 5) x
    
    // Question 3.8
    let rec sumPositive (x : int list) =
        match x with
        | [] -> 0
        | x :: xs when x < 0 -> sumPositive xs
        | x :: xs -> x + sumPositive xs
        ;;
        
    let sumPositive_2 x =
        List.fold(fun x acc -> if x > 0 then acc + x else acc ) 0 x
    
    let sumPositive_3 x = List.filter(fun x -> x > 0 ) x |> List.fold (fun acc x -> acc + x ) 0;;

  
    // Yellow Questions
    // Question 3.9
    // with pipeline 
    let add5mul3_3 (f: 'a -> int) (x: 'a) =  add5_2 (fun a -> x a) |> mul3_2;;
    
    let rec mergeFuns _  = failwith "not implemented"
        
    let rec facFuns _ = failwith "not implemented"
        
    let fac_2 _ = failwith "not implemented"


    // Question 3.11
    let removeOddIdx _ = failwith "not implemented"
        
    
    let weird _ = failwith "not implemented"
    
   
    let insert _= failwith "not implemented"
                
    let rec permutations _ = failwith "not implemented"