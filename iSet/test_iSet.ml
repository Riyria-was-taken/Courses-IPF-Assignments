(* Autor: Mateusz Gienieczko *)
open ISet;;

let info = false;;

let simple l =
  let (e, res) =
    List.fold_left (fun ((px, py), la) (x, y) ->
      if py + 1 >= x then ((px, max py y), la)
      else ((x, y), (px, py)::la)) ((List.hd l), []) (List.tl l)
  in
  List.rev (e::res);;

let long l =
  let rec add_inter acc (x, y) =
    if x == y then x::acc
    else add_inter (x::acc) (x + 1, y)
  in
  List.rev (List.fold_left (fun acc inter -> (add_inter [] inter) @ acc) [] l);;
  
let add_list =
  List.fold_left (fun s x -> add x s);;

let mem_all a l1 =
  List.filter (fun x -> not (mem x a)) l1 = []

let mem_none a l1 =
  List.filter (fun x -> mem x a) l1 = []

(* Small correctness tests *)

let l1 = [(-10, -8); (-7, -7); (-4, -1); (1, 1); (3, 7); (10, 15); (100, 1000)];;
let a = add_list empty l1;;

assert(elements a = simple l1);;
assert(mem_all a (long l1));;
assert(below 1000 a = 921);;

let (a1, b, a2) = split 4 a;;
assert(b);;
assert(simple (elements a1 @ [(4, 4)] @ elements a2) = simple l1);;
assert(List.filter (fun (x, y) -> y >= 4) (elements a1) = []);;
assert(List.filter (fun (x, y) -> x <= 4) (elements a2) = []);;

let (a1, b, a2) = split 3 a;;
assert(b);;
assert(simple (elements a1 @ [(3, 3)] @ elements a2) = simple l1);;
assert(List.filter (fun (x, y) -> y >= 3) (elements a1) = []);;
assert(List.filter (fun (x, y) -> x <= 3) (elements a2) = []);;

let (a1, b, a2) = split 2 a;;
assert(not b);;
assert(simple(elements a1 @ elements a2) = simple l1);;
assert(List.filter (fun (x, y) -> y >= 2) (elements a1) = []);;
assert(List.filter (fun (x, y) -> x <= 2) (elements a2) = []);;

let b = add (1, 10) a;;
let l2 = List.sort (fun (x1, _) (x2, _) -> compare x1 x2) ((1, 10)::l1);;

assert(elements b = simple l2);;

let c = remove (1, 10) a;;
let d = remove (1, 10) b;;

assert(elements c = elements d);;

let e = add (min_int, max_int) a;;
assert(elements e = [(min_int, max_int)]);;
assert(below 1 e = max_int);;

let f = remove (min_int, max_int) a;;
assert(elements f = []);;

let l3 = [(16, 99); (2, 2); (8, 9); (-6, -5)];;
let g = add_list a l3;;
assert(elements g = [(-10, -1); (1, 1000)]);;
assert(not (mem 0 g));;
let h = remove (420, 690) g;;
assert(not (mem 500 h));;
assert(elements h = [(-10, -1); (1, 419); (691, 1000)]);;
let i = add (0, 0) g;;
assert(elements i = [(-10, 1000)]);;
let j = remove (-9, -1) i;;
assert(elements j = [(-10, -10); (0, 1000)]);;
let k = remove (500, 999) j;;
assert(elements k = [(-10, -10); (0, 499); (1000, 1000)]);;

(* Performance tests *)

let rec aux l i =
  if i = 0 then l
  else aux (i::l) (i - 1);;

let l1 = snd (List.fold_left (fun (i, l) _ -> (i + 3, (i, i + 1)::l)) (min_int, [])
    (aux [] 100000));;

let l2 = snd (List.fold_left (fun (i, l) _ -> (i - 3, (i, i + 1)::l)) (max_int - 3, [])
	(aux [] 100000));;

let l3 = snd (List.fold_left (fun (i, l) _ -> (i + 3, (i, i + 1)::l)) (0, [])
	(aux [] 100000));;

let l4 = snd (List.fold_left (fun (i, l) _ -> (i - 3, (i, i + 1)::l)) (0, [])
	(aux [] 100000));;

if info then Pervasives.print_endline "Starting performence";;
let a = add_list empty l1;;
if info then Pervasives.print_endline "Added l1";;
let a = add_list a l1;;
if info then Pervasives.print_endline "Added l1";;
let a = add_list a l2;;
if info then Pervasives.print_endline "Added l2";;
let a = add_list a l2;;
if info then Pervasives.print_endline "Added l2";;
let a = add_list a l3;;
if info then Pervasives.print_endline "Added l3";;
let a = add_list a l3;;
if info then Pervasives.print_endline "Added l3";;
let a = add_list a l4;;
if info then Pervasives.print_endline "Added l4";;
let a = add_list a l4;;
if info then Pervasives.print_endline "Added l4";;

let test s (a, b) step i =
  let rec aux s (x, y) i =
    if i = 0 then s
    else aux (remove (x, y) s) (x + step, y + step) (i - 1)
  in
  aux s (a, b) i;;
    
test a (min_int + 1, min_int + 10000) 2 100000;;
if info then Pervasives.print_endline "Test 1";;
test a (max_int / 2, max_int / 2 + 10000) 2 100000;;
if info then Pervasives.print_endline "Test 2";;
test a (min_int + 10000, max_int / 2) 2 100000;;
if info then Pervasives.print_endline "Test 3";;
test a (max_int / 2, max_int - 1000000) 2 100000;;
if info then Pervasives.print_endline "Test 4";;
test a (max_int - 10000000, max_int - 1000000) 2 100000;;
if info then Pervasives.print_endline "Test 5";;

remove (min_int, max_int) a;;
if info then Pervasives.print_endline "Starting add";;
for i = 0 to 10000 do
  (fun _ -> ()) (add (min_int + i, max_int - i) a);
done;;

if info then Pervasives.print_endline "Starting remove";;
for i = 0 to 10000 do
  (fun _ -> ()) (remove (min_int + i, max_int - i) a)
done;;

if info then Pervasives.print_endline "Starting split";;
for i = 0 to 10000 do
  (fun _ -> ()) (split (min_int + i) a)
done;;

if info then Pervasives.print_endline "Starting below";;
for i = 0 to 10000 do
  (fun _ -> ()) (below (min_int + i) a)
done;;

(* Copyright Artur "mrowqa" Jamro 2015 *)
(* Copyright Marcin Mielniczuk 2015 *)
(* Released under the MIT license *)
(* almost rewritten from scratch, only used the codebase *)
(* You can modify 'n' and 'clear_steps' global parameters below. *)
(* After clear_steps steps, if this parameter is positive, *)
(* tested intervals are cleared. *)
(* If you have bug, please set debug = true to make manual debugging *)
(* possible *)

let debug = false
let info = false
let verbose = false
let n = 2*1000*1000

module Integers =
    struct
        type t = int
        let compare = Pervasives.compare
    end

module S = Set.Make(Integers)

let loop f (a,b) s =
    let rec pom a b acc =
        if a = b then (f a acc)
        else pom (a+1) b (f a acc)
    in pom a b s

let print_list l = List.iter (fun a -> Printf.printf "%d " a) l

module IntSet =
    struct
        include Set.Make(Integers)
        let add = loop S.add
        let remove = loop S.remove
    end

(* let's use global vars *)
let lo = if debug then 0 else -100000
let hi = if debug then 20 else 100000
let range = hi - lo
let clear_step = if debug then 10 else 0
let intset = ref IntSet.empty
let iset = ref ISet.empty
let rnd l h = Random.int (h-l+1) + l
let random () = Random.int range + lo

type testAction =
    | TestAdd
    | TestRemove
    | TestSplit
    | TestBelow

let sort (x, y) =
    if x < y then (x, y) else (y, x)

let interval_to_list (a,b) =
    List.rev (loop (fun x l -> x::l) (a,b) [])

let to_int_list ll =
    List.fold_left (fun acc el -> List.rev_append (List.rev acc) (interval_to_list el)) [] ll

let print_intset set =
    print_list (IntSet.elements set)

let print_iset set =
    print_list (to_int_list (ISet.elements set))

let print_sets () =
    print_string "\nPseudoSet: "; print_intset !intset;
    print_string "\n     iSet: "; print_iset !iset

let bt () = print_sets(); print_newline()

let get_action () : testAction =
    let a = Random.int 20 in
    if a < 8 then
        TestAdd
    else if a < 12 then
        TestRemove
    else if a < 16 then
        TestSplit
    else
        TestBelow

let test_add () : unit =
    let a, b = (random (), rnd 2 10) in
    intset := IntSet.add (a, a+b) !intset;
    iset := ISet.add (a, a+b) !iset;
    if info then Printf.printf "add (%d, %d)... " a (a+b);;

let test_remove () : unit =
    let a, b = (random (), rnd 5 20) in
    if info then Printf.printf "remove (%d, %d)... " a (a+b);
    intset := IntSet.remove (a, a+b) !intset;
    iset := ISet.remove (a, a+b) !iset

let test_split () : unit =
    let a = random ()
    and side = Random.int 2 in
    let sidetxt = if side = 0 then "below" else "above" in
        if info then Printf.printf "split %d, taking the ones %s... " a sidetxt;
        let b, c, d = IntSet.split a !intset in
        let bb, cc, dd = ISet.split a !iset in
        let t = [| b; d |] and tt = [| bb; dd |] in
        assert (c = cc);
        intset := t.(side);
        iset := tt.(side)

let test_below () : unit =
    let a = random () in
    if info then Printf.printf "below %d... " a;
    let test = ISet.below a !iset
    and b, _, _ = IntSet.split (a+1) !intset in
    let c = S.cardinal b in
    try assert (test = c)
    with Assert_failure x ->
        Printf.printf "\nReturned %d, expected %d\n" test c;
        if debug then bt ();
        raise (Assert_failure(x))

let check_correctness () : unit =
    if verbose then print_sets ();
    let ints = IntSet.elements !intset in
    let i = to_int_list (ISet.elements !iset) in
    begin
        try assert (ints = i)
        with Assert_failure x ->
            if debug then bt ();
            raise (Assert_failure(x))
    end;
    if info then 
        Printf.printf "- OK!\n"; flush stdout


let _ =
    Random.self_init ();
    if info then Printf.printf "Starting.\n"; flush stdout;
    let i = ref 0 in
    while !i < n do
      let () =
        if clear_step > 0 && !i mod clear_step = 0 then begin
            Printf.printf "[clear]\n";
            iset := ISet.empty;
            intset := IntSet.empty
        end;
        i := !i + 1;
        if info then Printf.printf "%d. " !i;
        match get_action () with
            | TestAdd -> test_add ()
            | TestRemove -> test_remove ()
            | TestSplit -> test_split ()
            | TestBelow -> test_below ()
        in check_correctness ()
    done

let zle = ref 0
let test (id:int) (result:bool) (expected:bool) : unit =
    if result <> expected then begin
        Printf.printf "Fail: %d\n" id;
        assert (false);
    end;;

open ISet;;



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

open ISet
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

(* Tests to iSet.ml *)

let zle = ref 0
let test n b =
  if not b then begin
    Printf.printf "Zly wynik testu %d!!\n" n;
    assert (false);
  end

open ISet;;

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

(* moje testy zadanie modyfikacje *)
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

(* Copyright Artur "mrowqa" Jamro 2015 *)
let zle = ref 0
let test (id:int) (result:bool) (expected:bool) : unit =
    if result <> expected then begin
        Printf.printf "Zly wynik testu %d!\n" id;
        incr zle
    end;;

open ISet;;



let s = empty;;
test 11 (is_empty s) true;;
test 12 (is_empty (add (1, 1) s)) false;;


(* niestety musimy zalozyc poprawnosc mem... *)

let s = add (10, 12) empty;;
test 21 (mem 9 s) false;;
test 22 (mem 10 s) true;;
test 23 (mem 12 s) true;;
test 24 (mem 13 s) false;;

let s = add (4, 7) s;;
test 25 (mem 8 s) false;;
test 26 (mem 11 s) true;;
test 27 (mem 5 s) true;;
test 28 (mem 3 s) false;;


let s = add (1, 1) (add (15, 16) (add (10, 14) (add (6, 9) empty)));;
test 31 (mem 10 (remove (10, 10) s)) false;;
test 32 (is_empty (remove (1, 20) s)) true;;
test 33 (mem 7 (remove (8, 15) s)) true;;

let s = add (-1, 1) (add (3, 7) (add (10, 12) (add (15, 18)
        (add (-15, -13) empty))));;
let s = remove (-10, 12) s;;
test 34 (is_empty s) false;;
test 35 (mem 5 s) false;;
test 36 (mem (-10) s) false;;
test 37 (mem (-15) s) true;;
test 38 (mem 17 s) true;;


test 41 (elements (add (4, 5) (add (7, 8) empty)) = [(4, 5); (7, 8)]) true;;
test 42 (elements (add (1, 1) (add (11, 14) (add (6, 9) (add (4, 5) empty))))
        = [(1, 1); (4, 9); (11, 14)]) true;;


let s = add (3, 4) (add (8, 10) (add (15, 20) empty));;
test 51 (below 2 s = 0) true;;
test 52 (below 3 s = 1) true;;
test 53 (below 10 s = 5) true;;
test 54 (below 15 s = 6) true;;
test 55 (below 100 s = 11) true;;
let s = add (1, max_int) (add (-1, 0) empty);;
test 56 (below max_int s = max_int) true;;
let s = add (-min_int, max_int) empty;;
test 57 (below max_int s = max_int) true;;
test 58 (below min_int s = 1) true;;


let s = add (3, 4) (add (8, 10) (add (15, 20) empty));;
let l, pres, r = split 9 s;;
test 61 (mem 9 l) false;;
test 62 (mem 9 r) false;;
test 63 (mem 8 l) true;;
test 64 (mem 10 r) true;;
test 65 pres true;;
test 66 (mem 7 l) false;;
test 67 (mem 4 l) true;;
test 68 (mem 11 r) false;;
test 69 (mem 16 r) true;;


let s = add (1, 1) (add (11, 14) (add (6, 9) (add (4, 5) empty)));;
let a = ref [];;
let foo x = a := x::!a; ();;
test 71 (iter foo s; !a = [(11, 14); (4, 9); (1, 1)]) true;;


let s = add (1, 1) (add (11, 14) (add (6, 9) (add (4, 5) empty)));;
let foo x a = x::a;;
test 81 (fold foo s [] = [(11, 14); (4, 9); (1, 1)]) true;;


let _ =
    if !zle = 0 then
        ()
    else
        Printf.printf "\nZlych odpowiedzi: %d.\n" !zle
;;
