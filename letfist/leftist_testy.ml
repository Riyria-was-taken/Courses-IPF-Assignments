(* Testy do zadania leftist. *)

open Leftist;;

let c = empty;;
let a = add 4 (add 1 c);;
let b = add 2 c;;

assert (not(is_empty b));;
assert (is_empty c);;
assert (not(compare a c = 0))

let z1 = delete_min a;;

assert (fst z1 = 1);;

let d = snd z1;;
let e = join a b;;
let z2 = delete_min e;;
let f = snd z2;;

assert (try let _ = delete_min c in false with Empty -> true);;
assert (fst z2 = 1);;
assert (fst (delete_min f) = 2);;
assert ((delete_min (snd (delete_min f))) = (4, empty));;

let e = add 5 e;;
let e = add 10 e;;
let e = add (- 5) e;;
let (x, g) = delete_min e;;
assert (x = (-5));;
let (x, g) = delete_min g;;
assert (x = 1);;
let (x, g) = delete_min g;;
assert (x = 2);;
let (x, g) = delete_min g;;
assert (x = 4);;
let (x, g) = delete_min g;;
assert (x = 5);;
let (x, g) = delete_min g;;
assert (x = 10);;
assert (try let _ = delete_min g in false with Empty -> true);;

let e = empty;;
let a = add "aa" (add "a" (add "zz" e));;
assert (is_empty e);;
assert (not(is_empty a));;
let (x, b) = delete_min a;;
assert (x = "a");;
let (x, b) = delete_min b;;
assert (x = "aa");;
let (x, b) = delete_min b;;
assert (x = "zz");;
assert (is_empty b);;

let a = add "abc" a;;
let a = add "xyz" a;;
let a = add "lol" a;;

let b = add "kappa" empty;;
let b = add "2137" b;;
let b = add "b" b;;
let b = add "cc" b;;

let c = add "aa" empty;;
let c = add "c" c;;
let c = add "olo" c;;

let d = join a b;;

let (x, d) = delete_min d;;
assert (x = "2137");;

let (x, d) = delete_min d;;
assert (x = "a");;

let (x, d) = delete_min d;;
assert (x = "aa");;

let d = join d c;;

let (x, d) = delete_min d;;
assert (x = "aa");;

let (x, d) = delete_min d;;
assert (x = "abc");;

let (x, d) = delete_min d;;
assert (x = "b");;

let (x, d) = delete_min d;;
assert (x = "c");;

let (x, d) = delete_min d;;
assert (x = "cc");;

let (x, d) = delete_min d;;
assert (x = "kappa");;

let (x, d) = delete_min d;;
assert (x = "lol");;

let (x, d) = delete_min d;;
assert (x = "olo");;

let (x, d) = delete_min d;;
assert (x = "xyz");;

let (x, d) = delete_min d;;
assert (x = "zz");;

assert (try let _ = delete_min d in false with Empty -> true);;

assert (is_empty d);;

let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;

let c = add 10 empty;;
let c = add (-5) c;;
let c = add 1 c;;
let c = add 4 c;;
let c = add 0 c;;

let b = join b c;;

let (a,b) = delete_min b;;
assert (a = (-5));;

let (a,b) = delete_min b;;
assert (a = (-1));;

let (a,b) = delete_min b;;
assert (a = 0);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 2);;

let (a,b) = delete_min b;;
assert (a = 3);;

let (a,b) = delete_min b;;
assert (a = 4);;

let (a,b) = delete_min b;;
assert (a = 10);;

assert (try let _= delete_min b in false with Empty -> true);;

let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;

let c = add 10 empty;;
let c = add (-5) c;;
let c = add 1 c;;
let c = add 4 c;;
let c = add 0 c;;

let b = join c b;;

let (a,b) = delete_min b;;
assert (a = (-5));;

let (a,b) = delete_min b;;
assert (a = (-1));;

let (a,b) = delete_min b;;
assert (a = 0);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 2);;

let (a,b) = delete_min b;;
assert (a = 3);;

let (a,b) = delete_min b;;
assert (a = 4);;

let (a,b) = delete_min b;;
assert (a = 10);;

assert (try let _= delete_min b in false with Empty -> true);;