let (pi,pi2) = Base.(pi,pi2)

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
  let u1, u2, u3 = Random.float 1., Random.float 1., Random.float 1. in 
  let a = 1. +. sqrt (1. +. 4. *. kappa *. kappa) in 
  let b = (a -. sqrt (2. *. a)) /. (2. *. kappa) in 
  let r = (1. +. b *. b) /. (2. *. b) in 
  let z = cos (pi *. u1) in 
  let f = (1 +. r *. z) /. (r + z)




















