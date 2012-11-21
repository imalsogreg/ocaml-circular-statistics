module V = Gsl.Vector

let weighted_mean theta_arr weights =
  if (V.length theta_arr <> V.length weights) then
    failwith "weighted_mean called with theta_arr and weights different lenghts"
  else
    let (x,y) = (ref 0., ref 0.) in 
    Base.phase_vector_iteri (fun i t -> 
      x := !x +. (weights.{i} *. cos t);
      y := !y +. (weights.{i} *. sin t)) theta_arr;
      Complex.arg {Complex.re = !x; Complex.im = !y}

let mean theta_arr = 
(* slower *)
(*  let weights = Array.make (Array.length theta_arr) 1. in 
  weighted_mean theta_arr weights *)
  
(* faster, but kind of a code copy-paste *)
  let (r,i) = (ref 0., ref 0.) in 
  Base.phase_vector_iter ( fun t ->
    r := !r +. cos t;
    i := !i +. sin t ) theta_arr;
  Complex.arg {Complex.re = !r; Complex.im = !i}




















