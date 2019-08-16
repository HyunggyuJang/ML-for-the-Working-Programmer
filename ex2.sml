(* Exercise 2.7 *)
type oldEngMoney = int * int * int;
fun oldEngMoneyAdd((po1,s1,pe1), (po2,s2,pe2)) =
    let val amount = ((po1 + po2)*20 + s1 + s2)* 12 + pe1 + pe2
    in (amount div (20 * 12), (amount mod (20 * 12)) div 12, amount mod 12)
    end

fun oldEngNeg(po, s, pe): oldEngMoney = (~po, ~s, ~pe)
fun oldEngMoneySub(money1, money2) = oldEngMoneyAdd(money1, oldEngNeg money2)
