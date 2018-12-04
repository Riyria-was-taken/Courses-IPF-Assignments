open ISet;;

let zle = ref 0
let test (id:int) (result:bool) (expected:bool) : unit =
    if result <> expected then begin
        Printf.printf "Fail: %d\n" id;
        assert (false);
    end;;

let s = empty;;
test 1 (is_empty s) true;;

let s = add (1, 1) (add (15, 16) (add (10, 14) (add (6, 9) empty)));;
test 2 (mem 10 (remove (10, 10) s)) false;;
test 3 (mem 7 (remove (8, 15) s)) true;;

let s = add (10, 12) empty;;
test 4 (mem 9 s) false;;
test 5 (mem 10 s) true;;
let s = add (14, 15) s;;
test 6 (mem 15 s) true;;
test 7 (mem 13 s) false;;

let s = add (-1, 1) (add (3, 7) (add (10, 12) (add (15, 18)
        (add (-15, -13) empty))));;
let s = remove (-10, 12) s;;
test 8 (mem (-10) s) false;;
test 9 (mem (-15) s) true;;

let s = add (3, 4) (add (8, 10) (add (15, 20) empty));;
test 10 (below 2 s = 0) true;;
test 11 (below 3 s = 1) true;;
test 12 (below 10 s = 5) true;;

let l, pres, r = split 9 s;;
test 13 (mem 9 l) false;;
test 14 (mem 9 r) false;;
test 15 (mem 8 l) true;;
test 16 (mem 10 r) true;;

let a = empty
let a = add (-20, 5) a
let a = add (6, 18) a
let a = add (4, 10) a
let a = add (14, 16) a
let a = remove (-18, 14) a
let a = remove (5, 17) a;;
assert(mem 14 a = false);;
let a = add (-4, 9) a;;
assert(mem 16 a = false);;
assert(mem (-14) a = false);;
assert(mem 10 a = false);;
let a = remove (-9, 10) a;;
let a = add (-6, 7) a;;
let a = add (-2, 7) a;;
let a = add (-12, 17) a;;
let a = add (-13, 8) a;;
let a = add (-13, -2) a;;
assert(mem 11 a = true);;
assert(elements a = [(-20, -19); (-13, 18)]);;

let zle = ref 0
let test n b =
  if not b then begin
    Printf.printf "Zly wynik testu %d!!\n" n;
    assert (false);
  end

let a = empty;;
let a = add (2, 5) a;;
let a = add (7, 10) a;;
let a = add (12, 20) a;;
let a = add (0, 0) a;;

test 1 (mem 1 a = false);;
test 2 (mem 2 a = true);;
test 3 (mem 9 a = true);;
test 4 (mem 21 a = false);;

let elem = elements a;;

test 5 (elem = [(0,0);(2,5);(7,10);(12,20)]);;
test 6 (below 6 a == 5);;
test 7 (below 10 a == 9);;
test 8 (below 19 a == 17);;

let (l,_,r) = split 15 a;;

test 9 (elements l = [(0,0);(2,5);(7,10);(12,14)]);;
test 10 (elements r = [(16,20)]);;

let (l,_,r) = split 8 a;;

test 11 (elements l = [(0,0);(2,5);(7,7);]);;
test 12 (elements r = [(9,10);(12,20)]);;


let a = add (6, 6) a;;
let b = add (11, 11) a;;

test 13 (elements a = [(0,0);(2,10);(12,20)]);;
test 14 (elements b = [(0,0);(2,20)]);;

let b = empty;;
let b = add (-10, 5) b;;
let b = add (10, 34) b;;
test 15 (elements b  = [(-10,5);(10,34)]);;

let b = add (22, 40) b;;
test 16 (elements b  = [(-10, 5);(10, 40)]);;

let b = add (41, 45) b;;
test 17 (elements b  = [(-10, 5);(10, 45)]);;

let b = add (80, 102) b;;
let b = add (130, 220) b;;
test 18 (elements b  = [(-10, 5);(10, 45);(80,102);(130,220)]);;

let b = add (45, 140) b;;
test 19 (elements b  = [(-10, 5);(10, 220)]);;

let c = empty;;
let c = add (4, max_int) c;;
let c = add (min_int, 0) c;;

test 20 (mem 4 c = true);;
test 21 (mem 0 c = true);;
test 22 (mem 20 c = true);;
test 23 (mem 2 c = false);;
test 24 (elements c = [(min_int, 0);(4, max_int)]);;
test 25 (below 0 c = max_int);;
test 26 (below max_int c = max_int);;


let d = empty;;
let d = add (min_int, max_int) d;;

test 27 (below 0 c = max_int);;
test 28 (below (-2) c = max_int);;
test 29 (below min_int c = 1);;
test 30 (below (min_int+1) c = 2);;

(*	(elements a = [(0,0);(2,10);(12,20)])
	(elements b  = [(-10, 5);(10, 220)])
	(elements c = [(min_int, 0);(4, max_int)]) *)

let a = remove (5,6) a;;
test 50 (elements a = [(0,0);(2,4);(7,10);(12,20)]);;
test 51 (below 11 a = 8);;
let (l,x,_) = split 1 a;;
test 52 ((l, x) = ((add (0,0) empty), false));;
let (_,x,r) = split 20 a;;
test 53 ((x, r) = (true, empty));;

let pom = a;;


let a = remove (12,19) a;;
test 54 (elements a = [(0,0);(2,4);(7,10);(20,20)]);;
test 55 (mem 19 a = false);;
test 56 (below 19 a = 8);;
test 57 (below 1 a = 1);;
test 58 (below 5 a = 4);;

let (l, x, r) = split 7 a;;
test 59 (x = true);;
test 60 (elements l = [(0,0);(2,4)]);;
test 61 (elements r = [(8,10);(20,20)]);;


let a = remove (1,1) a;;
test 62 (elements a = [(0,0);(2,4);(7,10);(20,20)]);;
test 63 (mem 19 a = false);;
test 64 (below 20 a = 9);;
test 65 (below 1 a = 1);;
test 66 (below 5 a = 4);;

let (l, x, r) = split 7 a;;
test 67 (x = true);;
test 68 (elements l = [(0,0);(2,4)]);;
test 69 (elements r = [(8,10);(20,20)]);;


let a = remove (0,20) a;;
test 70 (elements a = []);;
test 71 (below max_int a = 0);;
test 72 (is_empty a = true);;

(* elements pom = [(0,0);(2,4);(7,10);(12,20)] *)

let x = ref 0;;
let f (c, d) = 
	x := !x + c;;
iter f pom;;
test 100 (!x = 21);;

let x = fold (fun (c,d) acc -> acc + (d-c+1)) pom 0;;
test 101 (x = below 100 pom);;

let good = ref 0 and bad = ref 0

let check nr warunek wartosc =
  if warunek = wartosc then
    begin
      (* Printf.printf "OK - TEST nr %d \n" nr; *)
      incr good
    end
  else
    begin
      Printf.printf "Fail: %d\n" nr;
      assert (false);
    end;;

open ISet;;

let liczba a = List.length (elements a)

(* Testy na add i remove *)

let a = empty
let a = add (17, 20) a
let a = add (5, 8) a
let a = add (1, 2) a
let a = add (10, 12) a
let a = add (28, 35) a
let a = add (22, 23) a
let a = add (40, 43) a
let a = add (37, 37) a;;

check 1 (is_empty a) false;;
check 2 (mem 29 a) true;;
check 3 (mem 21 a) false;;
check 4 (mem 38 a) false;;
check 5 (mem 37 a) true;;
check 6 (below 8 a = below 9 a) true;;
check 7 (below 29 a) 17;;
check 8 (liczba a) 8;;

let a = add (37, 42) a;;

check 9 (liczba a) 7;;
check 10 (mem 37 a) true;;
check 11 (mem 38 a) true;;
check 12 (mem 39 a) true;;
check 13 (mem 40 a) true;;
check 14 (mem 41 a) true;;
check 15 (mem 42 a) true;;
check 16 (mem 44 a) false;;
check 17 (below 38 a = below 39 a) false;;

let tmp = remove (8, 22) a;;
let tmp = add (8, 22) tmp;;

check 18 (elements tmp = elements a) false;;

(* Testy na split *)

let (l, exists, p) = split 9 a;;

check 19 exists false;;
check 20 (liczba l) 2;;
check 21 (liczba p) 5;;
check 22 (mem 10 l) false;;
check 23 (mem 9 l) false;;
check 24 (mem 8 l) true;;
check 25 (mem 1 l) true;;
check 26 (mem 9 p) false;;
check 27 (mem 10 p) true;;
check 28 (mem 17 p) true;;
check 29 (mem 29 p) true;;
check 30 (mem 24 p) false;;
check 31 (mem 38 p) true;;
check 32 ((elements l @ elements p) = elements a) true;;

let (l, exists, p) = split 21 a;;

check 33 exists false;;
check 34 ((elements l @ elements p) = elements a) true;;

let (l, exists, p) = split 15 a;;
check 35 exists false;;
check 36 ((elements l @ elements p) = elements a) true;;


let b = empty
let b = add (5, 10) b
let b = add (40, 50) b
let b = add (20, 25) b
let b = add (12, 14) b
let b = add (17, 18) b
let b = add (52, 60) b
let b = add (62, 80) b
let b = add (83, 100) b;;

check 37 (mem 41 b) true;;
check 38 (mem 11 b) false;;

let d = empty;;
let (l, ex, p) = split 0 d;;

check 39 (is_empty l) true;;
check 40 (is_empty p) true;;

let d = add (17, 30) d;;
let d = add (1, 3) d;;
let d = add (10, 10) d;;
let d = remove (11, 11) d;;
let d = add (12, 14) d;;
let d = add (32, 35) d;;
let d = add (38, 40) d;;

check 41 (below 36 d = below 37 d) true;;

let d = add (36, 37) d;;

check 42 (below 36 d = below 37 d) false;;

let d = remove (37, 37) d;;
check 43 (below 36 d = below 37 d) true;;

let d = remove (20, 21) d;;

check 44 (elements d) [(1, 3); (10, 10); (12, 14); (17, 19); (22, 30); (32, 36); (38, 40)];;

let (l, ex, p) = split 15 d;;
check 144 (elements l) [(1, 3); (10, 10); (12, 14)];;
check 145 (elements p) [(17, 19); (22, 30); (32, 36); (38, 40)];;

check 45 ((elements l @ elements p) = elements d) true;;
check 46 (liczba l, liczba p) (3, 4);;

check 47 (mem 13 l) true;;
check 48 (mem 14 l) true;;
check 49 ex false;;

let (l, ex, p) = split 25 d;;

check 50 ex true;;
check 51 (elements l) [(1, 3); (10, 10); (12, 14); (17, 19); (22, 24)];;
check 52 (elements p) [(26, 30); (32, 36); (38, 40)];;

open ISet
let a = add (0, 5) empty;;
let a = add (7, 8) a;;
let a = add (-3, -3) a;;
let a = add (10, 13) a;;
assert(elements a = [(-3, -3); (0, 5); (7, 8); (10, 13)]);;
assert(below 8 a = 9);;
let b = add (6, 6) a;;
let b = remove (6, 6) b;;
let b = add (-100, -5) b;;
let b = add (-4, 6) b;;
assert(elements b = [(-100, 8); (10, 13)]);;
assert(below 10 b = 110);;
let c = remove (2, 10) a;;
assert(elements c = [(-3, -3); (0, 1); (11, 13)]);;
assert(below 12 c = 5);;
