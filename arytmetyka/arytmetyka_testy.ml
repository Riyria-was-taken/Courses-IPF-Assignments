(* Jagoda Kaminska *)
(* TESTY *)
	
	open Arytmetyka;;
	
    let aa = wartosc_dokladna 5.0;;
    
    assert (sr_wartosc aa = 5.0);;
    assert (min_wartosc aa = 5.0);;
    assert (max_wartosc aa = 5.0);;
    assert (not (in_wartosc aa (-. 5.35)));;
    
    let bb = wartosc_dokladnosc 30.0 5.0;;
    
    assert (min_wartosc bb = 28.5);;
    assert (max_wartosc bb = 31.5);;
    assert (sr_wartosc bb = 30.0);;
    assert (in_wartosc bb (31.2));;
    
    let cc = plus aa bb;;
    let dd = minus aa bb;;
    let ee = podzielic aa bb;;
    let ff = razy aa bb;;
    
    assert (min_wartosc cc = 33.5);;
    assert (max_wartosc cc = 36.5);;
    assert (sr_wartosc cc = 35.0);;
    assert (not(in_wartosc cc (32.2)));;
    
    assert (min_wartosc dd = -26.5);;
    assert (max_wartosc dd = -23.5);;
    assert (sr_wartosc dd = -25.0);;
    assert (not(in_wartosc dd (0.0)));;
    
    assert (min_wartosc ee = 5.0 /. 31.5);;
    assert (max_wartosc ee = 5.0 /. 28.5);;
    assert (in_wartosc ee (0.16));;

    assert (min_wartosc ff = 142.5);;
    assert (max_wartosc ff = 157.5);;
    assert (sr_wartosc ff = 150.0);;
    assert (in_wartosc ff 144.8);;

    let gg = wartosc_od_do (-. 21.0) 37.0;;
    
    assert (min_wartosc gg = -21.0);;
    assert (max_wartosc gg = 37.0);;
    assert (sr_wartosc gg = 8.0);;
    assert (not(in_wartosc gg (-. 77.0)));;
    
    let hh = wartosc_dokladna 0.0;;
    let ii = podzielic aa gg;;
    
    assert (min_wartosc ii = neg_infinity);;
    assert (max_wartosc ii = infinity);;
    assert (classify_float(sr_wartosc ii) = FP_nan);;
    assert (not (in_wartosc ii 0.0));;
    
    let jj = podzielic ii hh;;
    let kk = razy ii hh;;
    
    assert (classify_float (min_wartosc jj) = FP_nan);;
    assert (classify_float (max_wartosc jj) = FP_nan);;
    assert (classify_float (sr_wartosc jj) = FP_nan);;
    assert (not (in_wartosc jj 1.0));;
    assert (not (in_wartosc jj 0.0));;
    
    assert (min_wartosc kk = 0.0);;
    assert (max_wartosc kk = 0.0);;
    assert (sr_wartosc kk = 0.0);;
    assert (in_wartosc kk 0.0);;
    assert (not(in_wartosc kk (-. 5.32)));;
    assert (not (in_wartosc kk 502439.0));;
    
    let ll = plus ii (wartosc_dokladna 100000.0);;
    
    assert(in_wartosc ll 1000000000.0);;
    assert (min_wartosc ll = neg_infinity);;
    assert (max_wartosc ll = infinity);;
    assert (classify_float(sr_wartosc ll) = FP_nan);;
    
    assert (hh = kk);;

    assert(compare (razy jj aa) jj = 0);;
    assert(compare (podzielic jj bb) jj = 0);;
    assert(compare (plus jj cc) jj = 0);;
    assert(compare (minus jj hh) jj = 0);;
