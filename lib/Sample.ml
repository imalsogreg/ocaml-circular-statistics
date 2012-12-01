let (pi,pi2,(%)) = Base.(pi,pi2,(%))

let sign x = if x >= 0. then 1. else -1.

let circ_uniform_samp = Random.float pi2

let lin_uniform_samp range = 
  let d = snd range -. fst range in 
  Random.float d +. fst range

(* from http://www.uncarved.com/blog/practical_ocaml.mrk *)
let rec lin_normal_samp mu sig_sq =
  let u0, u1 = Random.float 2. -. 1., Random.float 2. -. 1. in 
  let s = u0 *. u0 +. u1 *. u1 in
  if s <= 1. 
  then (u0 *. sqrt (-2.0 *. log s /. s)) *. sqrt sig_sq +. mu
  else lin_normal_samp mu sig_sq
    

let vm_samp mu kappa =
  let a = 1. +. sqrt (1. +. 4. *. kappa *. kappa) in 
  let b = (a -. sqrt (2. *. a)) /. (2. *. kappa) in 
  let r = (1. +. b *. b) /. (2. *. b) in 
  let finalize u_3 f = (sign (u_3 -. 0.5) *. acos f +. mu) % pi2 in
  let rec aux () = 
    let u1, u2, u3 = Random.float 1., Random.float 1., Random.float 1. in 
    let z = cos (pi *. u1) in 
    let f = (1. +. r *. z) /. (r +. z) in
    let c = kappa *. (r -. f) in
    if c *. (2. -. c) -. u2 > 0.
    then finalize u3 f
    else begin
      if log10(c /. u2) +. 1. -. c < 0. 
      then aux ()
      else finalize u3 f
    end
  in 
  aux ()



















