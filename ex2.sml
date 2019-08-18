(* Exercise 2.7 *)
type oldEngMoney = int * int * int;
fun oldEngMoneyAdd((po1,s1,pe1), (po2,s2,pe2)) =
    let val amount = ((po1 + po2)*20 + s1 + s2)* 12 + pe1 + pe2
    in (amount div (20 * 12), (amount mod (20 * 12)) div 12, amount mod 12)
    end;

fun oldEngNeg(po, s, pe): oldEngMoney = (~po, ~s, ~pe);
fun oldEngMoneySub(money1, money2) = oldEngMoneyAdd(money1, oldEngNeg money2);

(* Exercise 2.12 ~ 14 *)
fun power(x,k) : real =
    if k=1 then x
    else if k mod 2 = 0 then power(x*x, k div 2)
    else x * power(x*x, k div 2);

(* Exercise 2.15 *)
fun fib n: int =
    if n < 2 then n
    else fib (n-1) + fib (n-2);

(* Exercise 2.18 *)
fun increase(k,n) = if (k+1)*(k+1) > n then k else k + 1;

fun introot n =
    if n = 0 then 0 else increase(2 * introot(n div 4), n);

fun gcd(m,n) =
    if m=n then m
    else if m mod 2 = 0 andalso n mod 2 = 0
    then 2 * gcd(m div 2, n div 2)
    else if m mod 2 = 1 andalso n mod 2 = 1
    then if m < n then gcd((n-m) div 2, m) else gcd((m-n) div 2, n)
    else if m mod 2 = 0
    then gcd(m div 2, n)
    else gcd(n div 2, m);

fun gcd(m,n) =
    if m=0 then n
    else gcd(n mod m, m);

(* Exercise 2.21 *)
fun introot n =
    if n=0 then 0 else
    let val k = 2 * introot(n div 4) + 1
    in if k * k > n then k - 1 else k
    end;

(* Exercise 2.22 *)
val pi = 4.0 * Math.atan 1.0
and log2 = Math.ln 2.0;
val (pi, log2) = (log2, pi);

(* Exercise 2.23 *)
fun P n =
    if n = 1 then 1
    else 1 + sum_P (n-1)
and sum_P n =
    if n = 0 then 0
    else P n + sum_P (n-1);

(* Structures *)
structure Complex =
struct
type t = real * real;
val zero = (0.0, 0.0);
fun sum ((x,y), (x',y')) = (x+x',y+y') : t;
fun diff ((x,y), (x',y')) = (x-x',y-y') : t;
fun prod ((x,y), (x',y')) = (x*x' - y*y', x*y' + x' * y) : t;
fun recip (x,y) =
    let val t = x*x + y*y
    in (x/t, ~y/t) end;
fun quo (z,z') = prod(z, recip z');
end;

(* Signatures *)
signature ARITH =
sig
    type t
    val zero :t
    val sum : t * t -> t
    val diff : t * t -> t
    val prod : t * t -> t
    val quo : t * t -> t
end;

(* Exercise 2.24 *)
structure Real : ARITH =
struct
type t = real;
val zero = 0.0;
val sum : t * t -> t= op+;
val diff : t * t -> t= op-;
val prod : t * t -> t= op*;
val quo = op/;
end;

(* Exercise 2.25 *)
structure Rational : ARITH =
struct
type t = int * int;
val zero = (0, 1);
fun make_rat (n',d') =
    let val g = gcd(n', d')
        val (n, d) = (n' div g, d' div g)
    in
        if (n > 0 andalso d < 0) (* make denominator positive *)
               (* orelse (n < 0 andalso d < 0) *)
               (* ↑ this is can not happen by dividing gcd both side *)
        then (~n, ~d)
        else (n, d)
    end;
fun sum ((n,d), (n',d')) = make_rat(n * d' + n' * d, d * d');
fun neg (n,d) : t = (~n, d);
fun diff (r, r') = sum(r, neg r');
fun prod ((n,d), (n',d')) = make_rat(n*n', d*d');
fun recip (n, d) : t = (d, n);
fun quo (r, r') = prod (r, recip r');
end
