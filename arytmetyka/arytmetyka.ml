(* autor: Jagoda Kaminska 406179 *)
(* reviewer: Kacper Kluk 406184 *)

open List

(*******************)    
(* DEKLARACJA TYPU *)
(*******************)

(* Typ reprezentujacy niedokladne wartosci:                              *)
(* rekord zawierajacy:                                                   *)
(*    -> najmniejsza: float - najmniejsza mozliwa do uzyskania wartosc;  *)
(*    -> najwieksza: float - najwieksza mozliwa do uzyskania wartosc;    *)
(*    -> przedzialy: int*int list - lista posortowanych przedzialow      *)
(*                                   mozliwych wartosci.                 *)

type wartosc =
{
    najmniejsza: float;
    najwieksza: float;
    przedzialy: (float * float) list;
}

(************************)
(* PROCEDURY POMOCNICZE *)
(************************)

(* Procedura pomocnicza, ktora sprawdza, czy przedzial (a, b) ma jakis koniec,*)
(* ktory jest nan-em.                                                         *)

let czy_pusty (a, b) = 
    if classify_float a = FP_nan then true
    else if classify_float b = FP_nan then true
    else false

(* Procedura pomocnicza, ktora zwraca mniejsza z dwoch liczb,                 *)
(* jezeli jedna z nich jest nan-em zwraca druga, a jesli obie, zwraca nan.    *)

let mini a b = 
    if classify_float a = FP_nan then b
    else if classify_float b = FP_nan then a
    else min a b
    
(* Procedura pomocnicza, ktora zwraca wieksza z dwoch liczb,                  *)
(* jezeli jedna z nich jest nan-em zwraca druga, a jesli obie, zwraca nan.    *)

let maxi a b = 
    if classify_float a = FP_nan then b
    else if classify_float b = FP_nan then a
    else max a b

(* W tej czesci przedzialy puste reprezentujemy jako pary (nan, nan)          *)
(* w dalszej czesci programu bedziemy je po prostu ignorowali.                *)

(* Procedura pomocnicza przyjmujaca przedzial i zwracajaca jego odwrotnosc    *)
(* Zwracany typ jest lista, gdyz moze reprezentowac dwa rozlaczne przedzialy  *)
(* W przypadku, gdy przedzial jest rowny (0., 0.), zwracany jest (nan, nan)   *)

let odwrotnosc (x, y) = 
    if (x *. y) > 0. then (1. /. y, 1. /. x) :: []
    else if x = 0. && y = 0. then (nan, nan) :: []
    else if x = 0. then (1. /. y, infinity) :: []
    else if y = 0. then (neg_infinity, 1. /. x) :: []
    else (neg_infinity, 1. /. x) :: (1. /. y, infinity) :: []



(*Pomocnicze procedury do operacji arytmetycznych na pojedynczych przedzialach*) 
(* Jako argument przyjmuja jeden spojny przedzial, a nie typ wartosc          *)
(* Zwracaja wyniki w postaci pary liczb, a nie typu wartosc                   *)
(* W przypadku dzielenia zwracana jest lista par,                             *)
(* bo wynik moze byc dwoma przedzialami                                       *)

(* Procedura pomocnicza zwracajaca wynik dodawania dwoch przedzialow          *)

let dodaj_przedzialy (a, b) (c, d) = (a +. c, b +. d)


(* Procedura pomocnicza zwracajaca wynik odejmowania dwoch przedzialow        *)

let odejmij_przedzialy (a, b) (c, d) = (a -. d, b -. c)


(* Procedura pomocnicza zwracajaca wynik mnozenia dwoch przedzialow           *)

let pomnoz_przedzialy (a, b) (c, d) = 
    if (a = infinity || a = neg_infinity) 
        && (b = infinity || b = neg_infinity)
        && (c = 0.) && (d = 0.) then (0., 0.)
    else if (c = infinity || c = neg_infinity) 
        && (d = infinity || d = neg_infinity)
        && (a = 0.) && (b = 0.) then (0., 0.)
    else
        let ll = a *. c
        and lp = a *. d
        and pl = b *. c
        and pp = b *. d
        in (mini ll (mini lp (mini pl pp)), maxi ll (maxi lp (maxi pl pp)))


(* Procedura pomocnicza zwracajaca wynik dzielenia dwoch przedzialow          *)  
(* Taka forma match-with jest mozliwa, poniewaz wiemy, ze odwrotnosc zwroci   *)
(* nam zawsze albo 1 albo 2 elementowa liste dwukrotek                        *)

let podziel_przedzialy (a, b) (c, d) =
    match (odwrotnosc (c, d)) with
    | h :: [] -> (pomnoz_przedzialy (a, b) h) :: []
    | h :: t -> 
        (pomnoz_przedzialy (a, b) h) :: (pomnoz_przedzialy (a, b) (hd t)) :: []
    | [] -> []
    
    
(* Procedura pomocnicza, ktora sortuje liste przedzialow,                       *)
(* scala przecinajace sie przedzialy,                                           *)
(* znajduje najmniejsza oraz najwieksza wartosc ktora mozemy uzyskac            *)
(* i zwraca te dane w postaci stalej typu wartosc.                              *)

let uprosc x = 
    let posortowana = sort compare x in
    (* Procedura pomocnicza, ktora otrzymuje niepusta posortowana liste       *)
    (* przedzialow i zwraca w postaci typu wartosc jednoczesnie scalajac      *)
    (* przecinajace sie przedzialy.                                           *)
    (* Zakladamy ze pierwszy element nie reprezentuje pustego przedzialu      *)
    let rec uprosc_helper (poc, kon) lista out min_w max_w =
        match lista with
        | h :: t ->
            if czy_pusty h then uprosc_helper (poc, kon) t out min_w max_w
            else let (akt_poc, akt_kon) = h in
                let akt_min_w = min (min min_w poc) akt_poc 
                and akt_max_w = max (max max_w kon) akt_kon in
                if kon >= akt_poc then 
                    uprosc_helper (poc, akt_kon) t out akt_min_w akt_max_w
                else uprosc_helper h t ((poc, kon) :: out) akt_min_w akt_max_w
        | [] -> 
            let akt_min_w = min min_w poc
            and akt_max_w = max max_w kon in
            {
                najmniejsza = akt_min_w; 
                najwieksza = akt_max_w; 
                przedzialy = rev ((poc, kon) :: out)
            }
    in 
    (* Procedura pomocnicza, usuwa wszystkie pary (nan, nan) z poczatku listy *)
    (* do momentu, az natrafi na inny przedzial.                              *)
    (* Jesli zas wszystkie pary z listy sa (nan, nan) to zwraca wartosc pusta.*)
    let rec usun_puste lista = 
        match lista with
        | [] -> []
        | h :: t -> if czy_pusty h then usun_puste t else lista
    in let lista_preprocesowana = usun_puste posortowana
        in match lista_preprocesowana with
        | [] ->
            {
                najmniejsza = nan;
                najwieksza = nan;
                przedzialy = [];
            }
        | (h::t) -> uprosc_helper h t [] infinity neg_infinity
        
(****************)
(* KONSTRUKTORY *)    
(****************)        
        
(* x, p -> [x -. x *. p%, x +. x *. p%] *)
        
let wartosc_dokladnosc x p = 
    let zakres = x *. p /. 100. in
    let a = x -. zakres and b = x +. zakres
    in
    {
        najmniejsza = min a b;
        najwieksza = max a b;
        przedzialy = (min a b, max a b) :: [];
    }

(* x, y -> [x, y] *)                
                            
let wartosc_od_do x y =
{
    najmniejsza = x;
    najwieksza = y;
    przedzialy = (x, y) :: [];
}
                        
(* x -> [x, x] *)    

let wartosc_dokladna x = 
{
    najmniejsza = x;
    najwieksza = x;
    przedzialy = (x, x) :: [];
}

(*************)    
(* SELEKTORY *)
(*************)            
        
(* Minimum wartosci *)

let min_wartosc x = x.najmniejsza

(* Maksimum wartosci *)

let max_wartosc x = x.najwieksza

(* Srednia arytmetyczna minimum i maksimum wartosci *)

let sr_wartosc x = 
    if x.najmniejsza = neg_infinity && x.najmniejsza = infinity then nan
    else ((x.najmniejsza +. x.najwieksza) /. 2.)
                
(* Sprawdza czy x znajduje sie w zbiorze wartosci w *)

let in_wartosc w x = 
    let rec petla x lista = 
        if lista = [] then false
        else let (a, b) = hd lista in
            if x >= a && x <= b then true
            else petla x (tl lista)
    in petla x w.przedzialy

(*****************************************)
(* MODYFIKATORY DOSTEPNE DLA UZYTKOWNIKA *)
(*****************************************)

(* Dodawanie dwoch wartosci *)

let plus x y = 
    let rec plus_helper a b out =
        if a = [] then out
        else
            match b with
            | [] -> plus_helper (tl a) y.przedzialy out
            | h :: t -> plus_helper a t ((dodaj_przedzialy (hd a) h) :: out)
    in uprosc (plus_helper x.przedzialy y.przedzialy [])
    
(* Odejmowanie dwoch wartosci *)
    
let minus x y = 
    let rec minus_helper a b out =
        if a = [] then out
        else
            match b with
            | [] -> minus_helper (tl a) y.przedzialy out
            | h :: t -> minus_helper a t ((odejmij_przedzialy (hd a) h) :: out)
    in uprosc (minus_helper x.przedzialy y.przedzialy [])

(* Iloczyn dwoch wartosci *)

let razy x y = 
    let rec razy_helper a b out =
        if a = [] then out
        else
            match b with
            | [] -> razy_helper (tl a) y.przedzialy out
            | h :: t -> razy_helper a t ((pomnoz_przedzialy (hd a) h) :: out)
    in uprosc (razy_helper x.przedzialy y.przedzialy [])

(* Iloraz dwoch wartosci *)

let podzielic x y = 
    let rec podzielic_helper a b out = 
        if a = [] then out
        else
            match b with
            | [] -> podzielic_helper (tl a) y.przedzialy out
            | h :: t -> 
                podzielic_helper a t ((podziel_przedzialy (hd a) h) @ out)
    in uprosc (podzielic_helper x.przedzialy y.przedzialy [])  
