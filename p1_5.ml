(* Problem 1 Solution *)
(* Author: Simon Liang *)

let a = [];;
let b = [1];;
let c = [2;3;4];;

let p1 x =
  match x with
  | [] -> 1
  | [head] -> 2
  | head::tail -> 0;;

p1 a;;
p1 b;;
p1 c;;

(* Problem 2 Solution *)
(* Author: Simon Liang *)

let a = [1];;
let b = [1;2;3;4];;

let rotate x=
  match x with
  | [] -> []
  | [head] -> [head]
  | head :: tail -> tail @ [head];;

let a = rotate a;;
let b = rotate b;;

(* Problem 3 Solution *)
(* Author: Simon Liang *)

let a = [];;
let b = [1];;
let c = [1;2;3];;

let rec rem_last x =
  match x with
  | [] -> []
  | [head] -> []
  | head::tail -> (head :: rem_last tail);;

rem_last a;;
rem_last b;;
rem_last c;;

(* Problem 4 Solution *)
(* Author: Simon Liang *)

let y = 3;;
let x = [1;2;3;4;3;5];;

let rec remove y list=
  match list with
  | [] -> list
  | head :: tail ->
    if head = y then (
      remove y tail;
    )
    else head::remove y tail;;

remove y x;;

(* Problem 5 Solution *)
(* Author: Simon Liang *)

let a = [0;1;0;1;2;1];;
let b = [];;
let c = [1];;

let find_zeroes_ones y =
  let rec count0_1 y(count0, count1)=
    match y with
    | [] -> (count0, count1)
    | head::tail ->
        if head = 0 then count0_1 tail(count0+1, count1)
        else if head = 1 then count0_1 tail(count0, count1 +1)
        else count0_1 tail(count0, count1)
  in
  count0_1 y(0,0);;

find_zeroes_ones a;;
find_zeroes_ones b;;
find_zeroes_ones c;;


