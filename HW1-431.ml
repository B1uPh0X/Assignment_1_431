
(* CS 431, 
Professor Andriamanalimanana, 
Homework #1, 
Questions 6, 7, 8, 9 *)

(* Problem #6 *)
(* Author: Karlo Mohammed *)

let rec makepairs x lst =
  match lst with
  | [] -> []
  | h :: t -> (x, h) :: makepairs x t;;


  (* Problem #7 *)
  (* Author: Karlo Mohammed *)

  let rec binomial n k =
    if k = 0 || k = n then 1  
    else binomial (n - 1) (k - 1) + binomial (n - 1) k ;;

  (* Problem #8 *)
  (* Author: Karlo Mohammed *)

  let rec dup lst =
    match lst with
    | [] -> []  
    | h :: t -> h :: h :: dup t ;;

  (* Problem #9 *)
  (* Author: Karlo Mohammed *)

  let rec undup lst =
    match lst with
    | [] -> []  
    | [x] -> raise (Failure "bad input")  
    | h1 :: h2 :: t ->
        if h1 = h2 then h1 :: undup t  
        else raise (Failure "bad input") ;;
  




  

