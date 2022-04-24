(* Good summary of inline tests here: https://ilyasergey.net/YSC2229/week-01-testing.html *)

let double x = x * 2
(* let%test _ = double 5 = 10 *)
(* let%test _ = double 2 = 4 *)

let cube x = x *. x *. x
(* let%test _ = cube 3.0 = 27.0 *)

let sign x s = if s = "pos" then x else if x = 0 then x else -x
(* let%test _ = sign 2 "neg" = -2 *)
(* let%test _ = sign 0 "neg" = 0 *)
(* let%test _ = sign 4 "pos" = 3 *)

let area r = r *. r *. Float.pi
(* let%test _ = area 3.0 = 28.274333882308138 *)

let rms x y = sqrt ((x *. x) +. (y *. y)) /. 2.

let is_valid_date d m =
  match m with
  | "Feb" -> 1 <= d && d <= 28
  | "Apr"
  | "Jun"
  | "Sep"
  | "Nov" ->
    1 <= d && d <= 30
  | "Jan"
  | "Mar"
  | "May"
  | "Jul"
  | "Aug"
  | "Oct"
  | "Dec" ->
    1 <= d && d <= 31
  | _ -> false

let rec fib n =
  if n = 1 || n = 2 then
    1
  else
    fib (n - 1) + fib (n - 2)

(* let%test _ = fib 3 = 2 *)
(* let%test _ = fib 7 = 13 *)

let fib_fast n =
  let rec h n pp p = if n = 1 then p else h (n - 1) p (pp + p) in
  h n 0 1

(* let%test _ = fib_fast 3 = 2 *)
let%test _ = fib_fast 7 = 13

let rec first_negative f n =
  let res = f n in
  if res < 0 then
    n
  else
    first_negative f (n + 1)

