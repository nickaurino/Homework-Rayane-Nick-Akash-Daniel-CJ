exception Negative_Amount

let change amount =
  if amount < 0 then raise Negative_Amount
  else
    let denominations = [ 25; 10; 5; 1 ] in
    let rec aux remaining denominations =
      match denominations with
      | [] -> []
      | d :: ds -> (remaining / d) :: aux (remaining mod d) ds
    in
    aux amount denominations

let first_then_apply array predicate consumer =
  match List.find_opt predicate array with None -> None | Some x -> consumer x

let powers_generator base =
  let rec generate_from power () =
    Seq.Cons (power, generate_from (power * base))
  in
  generate_from 1

let meaningful_line_count filename =
  let meaningful_line line =
    let trimmed = String.trim line in
    String.length trimmed > 0 && not (String.get trimmed 0 = '#')
  in
  let the_file = open_in filename in
  let finally () = close_in_noerr the_file in
  let rec count_lines count =
    try
      let line = input_line the_file in
      if meaningful_line line then count_lines (count + 1)
      else count_lines count
    with End_of_file -> count
  in
  Fun.protect ~finally (fun () -> count_lines 0)
(* Write your line count function here *)

(* Write your shape type and associated functions here *)

type shape = Sphere of float | Box of float * float * float

let volume s =
  match s with
  | Sphere r -> Float.pi *. (r ** 3.0) *. 4.0 /. 3.0 
  | Box (l, w, h) -> l *. w *. h 

let surface_area s =
  match s with
  | Sphere r -> 4.0 *. Float.pi *. (r ** 2.0) 
  | Box (l, w, h) -> 2.0 *. ((l *. w) +. (w *. h) +. (h *. l))


(* Write your binary search tree implementation here *)
type 'a binary_search_tree =
  | Empty
  | Node of 'a binary_search_tree * 'a * 'a binary_search_tree

let rec insert value tree =
  match tree with
  | Empty -> Node (Empty, value, Empty)
  | Node (left, v, right) ->
      if value < v then Node (insert value left, v, right)
      else if value > v then Node (left, v, insert value right)
      else tree

let rec size tree =
  match tree with
  | Empty -> 0
  | Node (left, _, right) -> 1 + size left + size right

let rec contains value tree =
  match tree with
  | Empty -> false
  | Node (left, v, right) ->
      if value = v then true
      else if value < v then contains value left
      else contains value right

let rec inorder tree =
  match tree with
  | Empty -> []
  | Node (left, v, right) -> inorder left @ [ v ] @ inorder right
