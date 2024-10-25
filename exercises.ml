(* Exception for negative amounts *)
exception Negative_Amount

(* Change function to break down an amount into denominations *)
let change (amount : int) : int list =
  if amount < 0 then
    raise Negative_Amount
  else if amount = 0 then
    [0; 0; 0; 0]  (* Special case for 0 *)
  else
    let denominations = [25; 10; 5; 1] in
    let rec aux (remaining : int) (denoms : int list) : int list =
      match denoms with
      | [] -> []
      | d :: ds -> (remaining / d) :: aux (remaining mod d) ds
    in
    aux amount denominations

(* first_then_apply function with strict type enforcement for predicate and transformation *)
let first_then_apply (lst : string list) (predicate : string -> bool) (f : string -> string) : string option =
  match List.find_opt predicate lst with
  | Some x -> Some (f x)
  | None -> None

(* Helper function: Checks if a string is non-empty *)
let non_empty (s : string) : bool = String.length s > 0

(* Helper function: Converts a string to lowercase *)
let lower (s : string) : string = String.lowercase_ascii s



(* Write your powers generator here *)

(* Write your line count function here *)

(* Write your shape type and associated functions here *)

(* Write your binary search tree implementation here *)

