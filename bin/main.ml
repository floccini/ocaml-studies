
(* printing stuff*)
let double_sum = [6;7;8] 
  |> List.map(fun x -> x * 2) 
  |> List.fold_left (+) 0;;

Printf.printf "%d" double_sum


(*quick sort function*)
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

move(1,2) 3 4
