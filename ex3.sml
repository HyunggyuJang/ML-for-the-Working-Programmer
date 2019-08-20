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

(* Exercise 3.11 *)
local
    val romans = ["M", "D", "C", "L", "X", "V", "I"];
    val rNumers = [1000, 500, 100, 50, 10, 5, 1];
    val romanPairs = ListPair.zip (romans, rNumers);
    fun romanNumeral (romanvals, 0) = ""
      | romanNumeral ((r,v) :: romanvals, amount) =
        if amount < v then romanNumeral (romanvals, amount)
        else r ^ romanNumeral ((r,v) :: romanvals, amount - v);
in
fun toRomanVerbose (amount) = romanNumeral(romanPairs, amount);
end;

(* or *)
local
    val romans = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"];
    val rNumers = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1];
    val romanPairs = ListPair.zip (romans, rNumers);
    fun romanNumeral (romanvals, 0) = ""
      | romanNumeral ((r,v) :: romanvals, amount) =
        if amount < v then romanNumeral (romanvals, amount)
        else r ^ romanNumeral ((r,v) :: romanvals, amount - v);
in
fun toRomanConcise (amount) = romanNumeral(romanPairs, amount);
end;

(* Exercise 3.14 *)
fun allChange (coins, coinvals, 0, results) = coins :: results
  | allChange (coins, [], amount, results) = results
  | allChange (coins, c :: coinvals, amount, results) =
    if amount < 0 then []
    else allChange(c :: coins,
                   c :: coinvals,
                   amount - c,
                   allChange(coins, coinvals, amount, results));
