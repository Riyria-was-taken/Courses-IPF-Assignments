(*
 * ISet - Interval sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl, Jacek Chrzaszcz
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** Interval Set.
    This is an interval set, i.e. a set of integers, where large
    intervals can be stored as single elements. Intervals stored in the
    set are disjoint. 
*)

(* Author: Jagoda Kaminska 406179 *)
(* Reviewer: Robert Michna 406247 *)

(* Type for AVL trees for intervals:                  *)
(* | Empty - empty tree                               *)
(* | Node(ls, (a, b), size, ps, height), where:       *)
(*  - ls and ps are left and right sons of the tree   *)
(*  - (a, b) is an interval in the root               *)
(*  - size is a number of values keeped in the tree   *)
(*  - height is a height of the tree                  *)
(* Invariant: intervals are disjoing in the tree.     *)
type t = Empty | Node of t * (int * int) * int * t * int

(* The empty set. *)
let empty = Empty

(* Returns true if the set is empty. *)
let is_empty tre = 
    tre = Empty

(* For safe addition when the sum is           *)
(* above max_int or below min_int.             *)
let plus x y =
    if x > 0 && y > 0 && (x + y) <= 0 then max_int
    else if x < 0 && y < 0 && (x + y) >= 0 then min_int
    else(x + y)
    
(* For safe substraction when the diffrence is *)
(* above max_int or below min_int.             *)
let minus x y =
    if x > 0 && y < 0 && (x - y) <= 0 then max_int
    else if x < 0 && y > 0 && (x - y) >= 0 then min_int
    else x - y

(* Returns the height of the tree *)
let height tre = 
    match tre with
    | Empty -> 0
    | Node(_, _, _, _, h) -> h
    
(* Returns the number of values keept in the tree. *)
let size tre = 
    match tre with
    | Empty -> 0
    | Node(_, _, siz, _, _) -> siz
 
(* Constructs a Node.                          *)
(* Expects correct left and right subtrees.    *)
let make l (b, e) r = 
    let tree_size = plus (plus (size l) (size r)) (plus 1 (minus e b))
    and tree_height = max (height l) (height r) + 1
    in Node(l, (b, e), tree_size, r, tree_height)

(* The balance function that optimizes the tree.*)
(* The result is a correct AVL tree.            *)
let bal l k r =
    let hl = height l in
    let hr = height r in
    if hl > hr + 2 then
        match l with
        | Node (ll, lk, _, lr, _) ->
           if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, _, lrr, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, _, rr, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, _, rlr, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l k r

(* Returns the minimal interval from the tree. *) 
let rec min_elt = function
  | Node (Empty, intv, _, _, _) -> intv
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found
 
(* Deletes the minimal interval from the tree, *)
(* the output is a correct AVL tree.           *)
 let rec remove_min_elt = function
  | Node (Empty, _, _, r, _) -> r
  | Node (l, k, _, r, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "ISet.remove_min_elt"

(* Merges two AVL trees, where the diffrence   *)
(* between the heights is below 4.             *)
let merge t1 t2 =
  match t1, t2 with
  | Empty, t | t, Empty -> t
  | _ ->
      let k = min_elt t2 in
      bal t1 k (remove_min_elt t2)

(* Checkes if x is in the interval (b, e).     *)
let if_in x (b, e) = 
    if (b <= x) && (x <= e) then 0
    else if x < b then - 1
    else 1

(* Full order of intervals which are disjoint. *)
let comp (a, b) (c, d) =
    if b < c then -1
    else if a = c && b = d then 0
    else 1

(* Adds one interval that is disjoint from all *)
(* other intervals in the set.                 *)
let rec add_unique (x, y) = function
    | Node (l, v, _, r, _) as tre ->
        let c = comp (x, y) v in
        if c = 0 then tre
        else if c < 0 then 
            let newltre = add_unique (x, y) l in
            bal newltre v r
        else 
            let newrtre = add_unique (x, y) r in
            bal l v newrtre
    | Empty -> Node (Empty, (x, y), minus (plus y 1) x, Empty, 1)

(* Merges two sets using the v interval.       *)
let rec join l v r = 
    match (l, r) with
    | (Empty, t) | (t, Empty) -> add_unique v t
    | (Node(ll, lv, _, lr, lh), Node(rl, rv, _, rr, rh)) ->
        if lh > rh + 2 then bal ll lv (join lr v r)
        else if rh > lh + 2 then bal (join l v rl) rv rr 
        else make l v r

(* [split x s] returns a triple [(l, present, r)], where                 *)
(* [l] is the set of elements of [s] that are strictly lesser than [x];  *)
(* [r] is the set of elements of [s] that are strictly greater than [x]; *)
(* [present] is [false] if [s] contains no element equal to [x],         *)
(*  or [true] if [s] contains an element equal to [x].                   *)
let split x tre =
    let rec loop x = function
    | Empty -> (Empty, false, Empty)
    | Node (l, (b, e), _, r, _) ->
        let v = (b, e) in
        let c = if_in x (b, e) in
        if c < 0 then 
            let (ll, czy, rl) = loop x l in (ll, czy, join rl v r)
        else if c > 0 then 
            let (lr, czy, rr) = loop x r in (join l v lr, czy, rr)
        else 
            if x = b && x = e then (l, true, r)
            else if x = b then (l, true, add_unique (x + 1, e) r)
            else if x = e then (add_unique (b, x - 1) l, true, r)
            else (add_unique (b, x - 1) l, true, add_unique(x + 1, e) r)
    in loop x tre

(* [mem x s] returns [true] if [s] contains [x], and [false] otherwise. *)
let mem x tre =
    let rec loop = function
    | Node(l, (b, e), _, r, _) ->
        let c = if_in x (b, e) in
        c = 0 || loop (if c < 0 then l else r)
    | Empty -> false
    in loop tre

(* [iter f s] applies [f] to all continuous intervals in the set [s]. *)
(*  The intervals are passed to [f] in increasing order.              *)
let iter f tre = 
    let rec loop = function
    | Empty -> ()
    | Node (l, k, _, r, _) -> loop l; f k; loop r
    in loop tre


(* [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)], where x1 *)
(*  ... xN are all continuous intervals of s, in increasing order. *)
let fold f tre acc = 
    let rec loop acc = function
    | Empty -> acc
    | Node(l, k, _, r, _) -> loop (f k (loop acc l)) r
    in loop acc tre

(* Return the list of all continuous intervals of the given set. *)
(* The returned list is sorted in increasing order.              *)
let elements tre =
    let rec loop a = function
    | Empty -> a
    | Node(l, v, _, r, _) -> loop (v :: loop a r) l
    in loop [] tre

(* If x belongs to some interval (b, e) from tre returns (b, e), *)
(* the output is (x, x) otherwise.                               *)
let get_interval x tre =
    let rec loop = function
    | Node(l, (b, e), _, r, _) ->
        let c = if_in x (b, e) in
        if c = 0 then (b, e)
        else if c < 0 then loop l
        else loop r
    | Empty -> (x, x)
    in loop tre

(* [add (x, y) s] returns a set containing the same elements as [s], *)
(*  plus all elements of the interval [[x,y]] including [x] and [y]. *)
(*  Assumes [x <= y].                                                *)
let add (x, y) tre = 
    let (bf, _) = 
        if mem (minus x 1) tre then get_interval (minus x 1) tre
        else get_interval x tre
    and (_, es) = 
        if mem (plus y 1) tre then get_interval (plus y 1) tre 
        else get_interval y tre in
    let (lower, _, helper) = split bf tre in
    let (_, _, higher) = split es helper in
    join lower (bf, es) higher

(* [remove (x, y) s] returns a set containing the same elements as [s], *)
(*  except for all those which are included between [x] and [y].        *)
(*  Assumes [x <= y].                                                   *)
let remove (x, y) tre =
    let (lower, _, helper) = split x tre in
    let (_, _, higher) = split y helper in
    match higher with
    | Empty -> lower
    | _ -> join lower (min_elt higher) (remove_min_elt higher)

(* [below n s] returns the number of elements of [s] that are lesser *)
(*  or equal to [n]. If there are more than max_int such elements,   *)
(*  the result should be max_int.                                    *)
let below x tre =
    if x = max_int then size tre
    else let (lower, _, _) = split (x + 1) tre in size lower
    
