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
