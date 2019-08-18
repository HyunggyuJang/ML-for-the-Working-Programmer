(* Exercise 3.1 *)
fun maxl l : int  =
    if null (tl l) then hd l
    else
        let val m = hd l
            val n = hd (tl l)
            val ntl = tl (tl l)
        in
            if m > n then maxl (m :: ntl)
            else maxl (n :: ntl)
        end;

(* Exercise 3.2 *)
fun last [e] = e
  | last (_ :: es) = last es;

(* Exercise 3.3 *)
fun take ([], _) = []
  | take (x::xs, i) =
    if i > 0 then x :: take (xs, i - 1)
    else [];

fun drop ([], _) = []
  | drop (x::xs, i) =
    if i > 0 then drop (xs, i - 1)
    else x::xs;

(* Exercise 3.4 *)
fun nth (x::xs, n) =
    if n > 0 then nth (xs, n - 1)
    else x;

(* Exercise 3.5 *)
infixr 5 @;
fun ([] @ ys) = ys
  | (xs @ [])  = xs
  | ((x::xs) @ ys) = x :: (xs @ ys);
