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

(* multiple arguments functions*)
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

(* currying 
process of transforming a function that receives multiple arguments in a sequency of functions that receives a single argument
this allows you to partially apply a function
*)

let sum x y = x + y;;

(* sum is a function that receives x and return a function, that function receives argument y and return the sum of x and y *)

let sum x = fun y -> x + y;;

sum 3 4;;
(* doing like that , sum 3 return a partial function that adds 3 to any value, then, this new function is applied to the argument 4 *)

(* partial application *)
(* add3 in this case turns into a function that adds 3 to any number *)
let add3 = sum 3;;
let result = add3 5;;

(* if we do not want to use curry, we create a function that receives all the arguments at once, using tuples *)
(* in this case, the function expects a single argument which is a tuple containing two numbers *)
let sum_tuple (x, y) = x + y;;

let multiply_by x y = x * y;;

let double = multiply_by 2;;

let result = double 5;;

Printf.printf "%d" result

(* prefix and infix *)
(* prefix functions is the regular form of working with functions, prefixing the function with its name followed by its parameters *)
sum 5 2;;

(* infix functions are use between its arguments, for example, arithmetic operators, like +, *, - and / *)
(* you can also create your own infixed functions like the following example, passing the operator first *)
(* useful for certains expressions, making them more readable *)

let add_1 = (+) 1;;

add_1 2;;

(* using the distance function shown before as an example to apply it as an infix function, we have: *)

(* |> and <| is our operatos to identify the function *)
let (|><|) x y = x -y |> abs;;

(* then we can use it as *)

3 |><| 2;;

3 |><| 2 |><| 6;;

(* 3 - 2 -> 1 -> 6 - 1 -> 5 *)

(* lambda functions *)
(* to initialize a lambda function, we use the word fun, followed by its arguments, a -> that separates the body of the function and the
declaration of the function
*)

List.map(fun x -> x * x) [1;2;3];;
(* [1;4;9] *)

(fun x y -> x - y |> abs) 20 35;;
(* 15 *)

(* recursive functions *)

(* to initialize a recursive function, we use the word rec *)

(* recursively removes an item from the list *)
(* [] -> 0 -> if the list is empty, return 0 *)
(* _::xs -> list with n elements, it separates the list and adding the first element to _, 
this element is called head, and the rest is called tail.
*)
let rec size = function
    | [] -> 0
    | _::xs -> 1 + size xs;;

size [1;2;5];;
(*
how it works:

size [1;2;5]
    1::[2;5] -> 1 + size [2;5]

size [2;5]
    2::[5] -> 1 + size [5]

size [5]
    5::[] -> 1 + 0

size [2;5]
    2::5 -> 1 + 1

size [1;2;5]
    1::[2;5] -> 1 + 2

result = 3

*)

(* another recursive example using factorial calculus *)

let rec factorial n =
  if n < 2 then
    1
  else
    n * fatorial (n - 1)


(* pipe operator *)
(* we can first pass the value then the function using the |> operator *)

(* useful when we want to do a sequence of function calls using the returned value from the previous function*)

7. |> sin;;

(* instead of *)

sin 7.;;

(* we can also use the backwards pipe operator, that basically passes the value to the previous declared function *)

(*in ocaml 4.1 or previous, this operator was <| but then it got changed to @@*)

sin @@ 2. + 1.;;

(* we can also mix both operators, which makes the funtion syntax similar to a infix function *)
(* min is a built-in function that receives 2 int and returns the lowest int *)
min 10 5;;

7 |> min @@ 2;;


(* function composition *)
(* composition is when we connect the output of a function to the input of another one, and store that in a new function.
*)

(* added the definitions for << and >> since they are not built-int OCaml operators *)
let (<<) f g x = f(g(x));;
let (>>) f g x = g(f(x));;

let minus x y = x - y |> abs;;

let minus_one = minus 1;;

let multiply x y = x * y;;

let byTwo = multiply 2;;

(* examples without composition *)

minus_one 9;;
(* 8 *)

 byTwo 8;;
(* 16 *)

(* using composition *)

let minusOneThenMultiply = byTwo << minus_one;;

minusOneThenMultiply 9;;
(* 16 *)

let multiplyThenMinusOne = byTwo >> minus_one;;

multiplyThenMinusOne 9;;
(* 17 *)