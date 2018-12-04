(* Autor: Jagoda Kaminska       *)
(* Reviewer: Antoni Zewierzejew *)

(* Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta *)
exception Empty

(* Typ reprezentujacy zlaczalne kolejki priorytetowe obiektow typu a': *)
(* Typ wariatowy z wariantami:                                         *)
(* Null - oznacza pusta kolejke (drzewo lewicowe),                     *)
(* Node - krotka zawierajaca:                                          *)
(* (wartosc, lewe poddrzewo, prawe poddrzewo, dlugosc prawej sciezki.  *)
type 'a queue = 
    | Null
    | Node of ('a * 'a queue * 'a queue * int)
            
(* Konstruktor pustej kolejki. *)                      
let empty = Null

(* Selektor, ktory sprawdza czy kolejka jest pusta. *)
let is_empty que =
    match que with
    | Null -> truea
    | _ -> false

(* Funkcja pomocnicza, ktora zwraca dlugosc najbardziej prawej sciezki. *)
let depth que = 
    match que with
    | Null -> 0
    | Node(_, _, _, d) -> d

(* Modyfikator, ktory laczy dwie kolejki w jedna i ja zwraca. *)
let rec join que1 que2 = 
    match que1, que2 with
    | Null, w | w, Null -> w
    | Node(a, lson_1, rson_1, _), Node(b, _, _, _) ->
        if a > b then
            join que2 que1
        else
            let temporary = join rson_1 que2 in 
            if depth lson_1 >= depth temporary then 
                Node(a, lson_1, temporary, depth temporary + 1)
            else
                Node(a, temporary, lson_1, depth lson_1 + 1)
    
(* Modyfikator, ktory dodaje wartosc do kolejki. *)
let add value que =
    let temporary = Node(value, Null, Null, 1) in 
    join temporary que

(* Modyfikator, ktory dla niepuste kolejki q zwraca pare:                  *)
(* (e, q'), gdzie e to element minimalny kolejki q, q' to q z usunietym e. *)
(* Gdy q jest pusta podnosi wyjatek Empty.                                 *)
let delete_min que =
    match que with
    | Null -> raise Empty
    | Node(value, left, right, _) -> (value, join left right)
