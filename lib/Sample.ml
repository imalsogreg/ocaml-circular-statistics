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

let simple_sample d =
  let module D = Distribution in
  match d with
      D.LinUniform (D.Constant s0, D.Constant s1) -> lin_uniform_samp (s0, s1)
    | D.LinNormal  (D.Constant mu, D.Constant sig_sq) -> lin_normal_samp mu sig_sq
    | D.VonMises   (D.Constant mu, D.Constant kappa) -> vm_samp mu kappa
    | _ -> failwith "Not implemented yet."

let dependent_sample d xs =
  let module D = Distribution in
  let eval_param p v = match p with D.Constant k -> k 
    | D.Dependent _ when xs = [] -> failwith "dependent_sample called without x value"
    | D.Dependent f -> f (List.hd xs) 
  in
  match d with
      D.LinUniform (p_s0, p_s1) -> lin_uniform_samp (eval_param p_s0 xs, eval_param p_s1 xs)
    | D.LinNormal (p_s0, p_s1) -> lin_normal_samp (eval_param p_s0 xs) (eval_param p_s1 xs)
    | D.VonMises (p_s0, p_s1) -> vm_samp (eval_param p_s0 xs) (eval_param p_s1 xs)
    | _ -> failwith "Not implemented yet."

let joint_sample d =
  List.fold_right (fun d_iv acc -> (dependent_sample d_iv acc):: acc ) d []


let xs_ys_of_n_samples (d: Distribution.distribution list) n =
  let rec n_samps n' acc = 
    match n' with
        0 -> acc
      | n'' -> n_samps (n''-1) (joint_sample d :: acc) in
  let rec transpose list = 
    try List.map List.hd list :: transpose (List.map List.tl list) with _ -> [] in
  let samps_tnsp = transpose (n_samps n []) in
  (List.nth samps_tnsp 1, List.nth samps_tnsp 0)




















