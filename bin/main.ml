(* printing stuff *)
let double_sum = [6;7;8] 
  |> List.map(fun x -> x * 2) 
  |> List.fold_left (+) 0;;

Printf.printf "%d" double_sum


(* quick sort function *)
let rec quicksort = function
  | [] -> []
  | x :: xs ->
      let smaller = List.filter((>) x) xs in
      let bigger = List.filter((<) x) xs in
          quicksort smaller @ [x] @quicksort bigger;;

quicksort [5; 3; 8; 1; 4]


(*custom data type*)
type point = int * int

let move ((x ,y): point) distance_x distance_y : point = (x + distance_x, y + distance_y);;

move(1,2) 3 4;;

(* constant functions *)
let f x = 3 (* since x is unused, it can be omitted like the following two examples *)
let f _ = 3
let f () = 3

(* This is different than simply staticly assigning 3 to f, while let f = 3 is a static binding, let f () = 3 will always recalculate
the value after the = sign, it is specially important in cases when the assigned value, in our case, 3, is mutable.
In this cases when we want to write a value as a function and not by directly assigning it to a variable, that's how we do it.
*)

(* linear functions *)
let f x = 2 * x;;

(* quadratic functions *)
(* calculates the exponentiation of a given float number *)
let f x = x ** 2.;;

(* trigonometric functions *)
(* calculates the sine of a given float number *)
sin(5.)

(* single argument functions *)
let square x = x * x;;
square 3;;

(* square = 9 *)

(* even if the function only accepts a single argument, we can still do a few different operations with it, like adding a sum *)

square 3 + 1;;
(* which results in square = 10 *)

(*but if we properly want to make the 1 as part of the equation, we can pass the sum of those two values as the argument for the function *)

square (3 + 1);;
(* which results in square = 16 *)

(* multiple arugments functions*)
let distance x y = x - y |> abs;;

distance 5 3;;

(* calculates the distance between two points, and pass its results to the abs function that returns the absolute value*)

(* high order functions *)

List.map;;

(* Taking List.map as a function, it is a function that in its first argument receives another function, so its signature is as follows:
(* - : ('a -> 'b) -> 'a list' -> 'b list' = <fun>*)
Receives the following arguments
1. Takes a function with a argument of type 'a and return of type 'b
2. A list of type 'a
Returns:
1. A list of type 'b

With that, the function receives a list of values of type 'a, apply the function that transforms type 'a into 'b and retorn a list of type 'b.
Besides that, List.map is a polimorphic function since it can be used with whatever type the data you are sending is.
*)