(** [from i j l] is the list containing the integers from [i] to [j],
    inclusive, followed by the list [l].
    Example:  [from 1 3 [0] = [1; 2; 3; 0]] *)
let rec from i j l = if i > j then l else from i (j - 1) (j :: l)

(** [i -- j] is the list containing the integers from [i] to [j], inclusive. *)
let ( -- ) i j = from i j []

(* product [★ ★ ] *)
let rec product = function
  | [] -> 1
  | x :: xs' -> x * product xs'

(* concat [★ ★ ] *)
let rec concat = function
  | [] -> ""
  | x :: xs' -> x ^ concat xs'

(* patterns [★ ★ ★ ] *)
let has_bigred_fst = function
  | "bigred" :: _ -> true
  | _ -> false

let two_or_four xs =
  let rec aux acc = function
    | [] -> acc
    | _ :: xs' -> aux (acc + 1) xs' in
  let count = aux 0 xs in
  match count with
  | 2
  | 4 ->
    true
  | _ -> false

let fst_two_eq = function
  | []
  | [_] ->
    false
  | x :: y :: _ -> x = y

(* library [★ ★ ★ ] *)
let fifth xs = if List.length xs < 5 then 0 else List.nth xs 4
let sort_desc xs = List.sort compare xs |> List.rev

(* library puzzle [★ ★ ★ ] *)
let lst xs = List.nth xs (List.length xs - 1)

let any_zeroes xs =
  let x = List.find_opt (fun x -> x = 0) xs in
  if x = None then false else true

(* take drop [★ ★ ★ ] *)
let take n xs =
  let rec aux acc xs n =
    if n = 0 then
      acc
    else
      match xs with
      | [] -> acc
      | [hd] -> acc @ [hd]
      | hd :: tl -> aux (acc @ [hd]) tl (n - 1) in
  aux [] xs n

let drop n xs =
  let rec aux acc xs n =
    if n > 0 then
      match xs with
      | [] | [_] -> []
      | _ :: tl -> aux acc tl (n - 1)
    else
      match xs with
      | [] -> acc
      | [hd] -> acc @ [hd]
      | hd :: tl -> aux (acc @ [hd]) tl (n - 1) in
  aux [] xs n
