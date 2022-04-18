fun sumDigits n = 
    if n < 10 then n
    else
        n mod 10 + sumDigits(n div 10);

fun additivePersistenceHelper n count = 
    if n < 10 then count
    else 
        additivePersistenceHelper (sumDigits n) (count + 1);

fun additivePersistence n = additivePersistenceHelper n 0;

fun digitalRoot n =
    if n < 10 then n
    else
        digitalRoot (sumDigits n);

fun alternateHelper [] sign = 0
    | alternateHelper (n :: ns) sign = (sign * n) + (alternateHelper ns (~1*sign));

fun alternate li = alternateHelper li 1;

fun alternate2Helper [] f g arg sign = arg 0
    | alternate2Helper (n :: []) f g arg sign = arg n
    | alternate2Helper (n :: ns) f g arg sign = 
        if sign = 1 then alternate2Helper ns f g (f (arg n)) (~1*sign)
        else
            alternate2Helper ns f g (g (arg n)) (~1*sign);

fun alternate2 li f g = alternate2Helper li f g (fn n => n) 1;


fun scan_left f y [] = [y]
    | scan_left f y (x::xs) = y :: scan_left f (f x y) xs;


fun zipRecycle ([], []) = []
    | zipRecycle ([], (y :: ys)) = []
    | zipRecycle ((x :: xs), []) = []
    | zipRecycle ((x :: xs), (y :: ys)) = (x, y) :: zipRecycle (xs, ys);



