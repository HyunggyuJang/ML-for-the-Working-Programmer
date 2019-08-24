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
(* Section 3.8 *)
signature ARITH =
sig
    type t
    val zero : t
    val sum : t * t -> t
    val diff : t * t -> t
    val prod : t * t -> t
    val quo : t * t -> t
end;

structure Bin : ARITH =
struct
type t = int list

val zero = [0];

fun carry (0, ps) = ps
  | carry (1, []) = [1]
  | carry (1, p :: ps) = (1-p) :: carry (p, ps);

local
    fun sum_ (c, [], qs) = carry (c, qs)
      | sum_ (c, ps, []) = carry (c, ps)
      | sum_ (c, p :: ps, q :: qs) = ((c + p + q) mod 2) :: sum_((c + p + q) div 2, ps, qs);
in fun sum (ps, qs) = sum_ (0, ps, qs);
end;

fun neg [] = [] : int list
  | neg (p :: ps) = ~p :: (neg ps);

fun borrow (0, ps) = ps
  | borrow (1, []) = [~1]
  | borrow (1, p :: ps) = (1-p) :: borrow ((1-p), ps);

local
    fun diff_ (b, [], qs) = neg (carry (b, qs))
      | diff_ (b, ps, []) = borrow (b, ps)
      | diff_ (b, p :: ps, q :: qs) = ((p - q - b) mod 2) :: diff_(~((p - q - b) div 2), ps, qs);
in fun diff (ps, qs) = diff_ (0, ps, qs);
end;

fun prod ([], _) = []
  | prod (0::ps, qs) = 0::prod(ps, qs)
  | prod (1::ps, qs) = sum(qs, 0::prod(ps, qs));


local
fun zeroPed (qs, 0) = qs
  | zeroPed (qs, n) = 0 :: zeroPed (qs, n-1);

fun isNeg [~1] = true
  | isNeg [_] = false
  | isNeg (q :: qs) = isNeg qs;

fun quorem_ (ps, qs, n, quot) =
    if n < 0 then (quot, ps)
    else let val qs' = zeroPed (qs, n)
             val rem = diff (ps, qs')
         in if (isNeg rem) then
                if (null quot) then quorem_ (ps, qs, n-1, quot)
                else quorem_ (ps, qs, n-1, 0::quot)
            else quorem_ (rem, qs, n-1, 1::quot)
         end;
in fun quorem (ps, qs) =
       let val lp = length ps
           val lq = length qs
       in if lp < lq then (zero, ps)
          else quorem_ (ps, qs, lp - lq, [])
       end;
end

fun quo (ps, qs) = #1 (quorem (ps, qs));

local fun stripZeros (b::[0]) = [b]
        | stripZeros (b::[x]) = b::[x]
        | stripZeros (b :: b1 :: b2 :: bs) = stripZeros (b :: (stripZeros (b1 :: b2 :: bs)));
in fun rem (ps, qs) = stripZeros (#2 (quorem (ps, qs)));
end;
end;

(* Exercise 3.15 *)

(* structure BinBool : ARITH = *)
(* struct *)
(* val zero = [false]; *)
(* fun carry (false, ps) = ps *)
(*   | carry (true, []) = [true] *)
(*   | carry (true, p :: ps) = (not p) :: carry (p, ps); *)

(* infix xor; *)
(* fun b xor b' = (b andalso not b') orelse (not b andalso b'); *)

(* local *)
(* fun sum_ (c, [], qs) = carry (c, qs) *)
(*   | sum_ (c, ps, []) = carry (c, ps) *)
(*   | sum_ (false, p :: ps, q :: qs) = opxor (p, q) :: sum_(p andalso q, ps, qs) *)
(*   | sum_ (true, p :: ps, q :: qs) = (not opxor (p, q)) :: sum_(p orelse q, ps, qs); *)
(* in sum (ps, qs) = sum_ (false, ps, qs); *)
(* end; *)

(* fun neg [] = [] *)
(*   | neg (p :: ps) = (not p) :: (neg ps); *)

(* fun borrow (false, ps) = ps *)
(*   | borrow (true, []) = [false] *)
(*   | borrow (true, p :: ps) = (not p) :: borrow (not p, ps); *)

(* local fun diff_ (b, [], qs) = neg (carry (b, qs)) *)
(*         | diff_ (b, ps, []) = borrow (b, ps) *)
(*         | diff_ (b, p :: ps, q :: qs) = *)
(*           (b xor (p xor q)) :: diff_ ((b andalso q) orelse (not p andalso (b xor q)), ps, qs); *)
(* in fun diff (ps, qs) = diff_ (false, ps, qs) *)
(* end; *)

(* fun prod ([], _) = [] *)
(*   | prod (false::ps, qs) = false::prod(ps, qs) *)
(*   | prod (true::ps, qs) = sum(qs, false::prod(ps, qs)); *)

(* fun zeroPed (ps, 0) = ps *)
(*   | zeroPed (ps, n) = false :: (zeroPed (ps, n - 1)); *)

(* fun isNeg [] = false *)
(*   | isNeg [false] = true *)
(*   | isNeg (p :: ps) = isNeg ps; *)

(* local fun quorem_ (ps, qs, n, quot) = *)
(*           if n < 0 then (quot, ps) *)
(*           else let val qs' = zeroPed (qs, n) *)
(*                    val rem = diff (ps, qs') *)
(*                in if (isNeg rem) then *)
(*                       if (null quot) then quorem_ (ps, qs, n-1, quot) *)
(*                       else quorem_ (ps, qs, n-1, false :: quot) *)
(*                   else quorem_ (rem,qs,n-1,true :: quot) *)
(*                end; *)
(* in fun quorem (ps, qs) = *)
(*        let val lp = length ps *)
(*            val lq = length qs *)
(*        in if lp < lq then (zero, ps) *)
(*           else quorem_ (ps, qs, lp - lq, []) *)
(*        end; *)
(* end; *)


(* end; *)

(* Exercise 3.18 *)
(* →binary *)
fun toBinary 0 = []
  | toBinary n = (n mod 2) :: (toBinary (n div 2));

(* →decimal *)
fun toDecimal [] = 0
  | toDecimal (b::bs) = b + 2 * (toDecimal bs);

fun toBinarySeq [] = []
  | toBinarySeq (d::ds) = Bin.sum(toBinary d,
                                  Bin.prod(toBinary 10,
                                           toBinarySeq ds));

fun decimal_add (d', []) = [d']
  | decimal_add (0, ds) = ds
  | decimal_add (d', d::ds) = ((d' + d) mod 10) :: decimal_add ((d' + d) div 10, ds);

fun decimal_times (n,[]) = []
  | decimal_times (n,d::ds) = ((n*d) mod 10) :: decimal_add ((n*d) div 10,
                                                             decimal_times (n,ds));

fun toDecimalSeq [] = []
  | toDecimalSeq (b::bs) = decimal_add (b,
                                        decimal_times (2,toDecimalSeq(bs)));


fun factorial 1 = [1]
  | factorial n = decimal_times (n,factorial (n-1));

fun stringList xs = String.concatWith "" (map Int.toString xs);
fun stripZero (0::xs) = xs
  | stripZero xs = xs;
fun stringFactorial n = stringList (stripZero (rev (factorial n)));
