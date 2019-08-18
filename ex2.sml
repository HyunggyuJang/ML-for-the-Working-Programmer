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

(* Exercise 2.21 *)
fun introot n =
    if n=0 then 0 else
    let val k = 2 * introot(n div 4) + 1
    in if k * k > n then k - 1 else k
    end;
