exception Negative_Amount

(* Change function to break down an amount into denominations, raising an exception if the amount is negative *)
let change amount =
  if amount < 0 then
    raise Negative_Amount  (* Directly raise Negative_Amount for negative input *)
  else
    (* List of denominations: quarters (25), dimes (10), nickels (5), pennies (1) *)
    let quarters = amount / 25 in
    let remaining_after_quarters = amount mod 25 in
    let dimes = remaining_after_quarters / 10 in
    let remaining_after_dimes = remaining_after_quarters mod 10 in
    let nickels = remaining_after_dimes / 5 in
    let pennies = remaining_after_dimes mod 5 in
    [quarters; dimes; nickels; pennies]



(* first_then_apply function that directly returns the transformed element or raises an exception if no match *)
let first_then_apply (lst : 'a list) (predicate : 'a -> bool) (f : 'a -> 'b) : 'b =
  match List.find_opt predicate lst with
  | Some x -> f x          (* Directly return the transformed value *)
  | None -> failwith "No element satisfies the predicate"  (* Raise an error if no match is found *)

(* Helper function: Checks if a string is non-empty *)
let non_empty (s : string) : bool = String.length s > 0

(* Helper function: Converts a string to lowercase *)
let lower (s : string) : string = String.lowercase_ascii s

(* Helper function: Checks if the length of a string is greater than 3 *)
let length_greater_than_3 (s : string) : bool = String.length s > 3

(* Helper function: Squares an integer *)
let square (n : int) : int = n * n

(* Change function with a limit on list size *)
let change (n : int) : int list =
  let limit = 4 in
  if n = 0 then [0; 0; 0; 0] (* Special case for 0 *)
  else List.init (min limit (abs n)) (fun _ -> n)





(* Write your powers generator here *)

(* Write your line count function here *)

(* Write your shape type and associated functions here *)

(* Write your binary search tree implementation here *)
