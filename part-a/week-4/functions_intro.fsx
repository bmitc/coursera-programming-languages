let double x = 2 * x

let incr x = x + 1

let aTuple = (double, incr, double(incr 7))

let first (x,_,_) = x

let eighteen = (first aTuple) 9