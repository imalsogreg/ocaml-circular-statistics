let lin_sum = Base.phase_vector_fold_left (+.) 0.

let lin_mean xs = 
  let n = float_of_int (Base.phase_vector_length xs) in
  (lin_sum xs ) /. n

let pair_mean (a,b) = (a +. b) /. 2.

let add_offset xs x0 = Base.phase_vector_map ((+.) x0) xs

let subtract_offset xs x0 =
  Base.phase_vector_map (fun x -> x -. x0) xs

let trd (_,_,c) = c
let fourth (_,_,_,d) = d

let max2 f range_x range_y =
  let max_y = fst ( Max1D.brent (fun y -> 
    (snd (Max1D.brent (fun x -> f x y) (fst range_x) (snd range_x)))) 
                      (fst range_y) (snd range_y)) in
  let max_x = fst (Max1D.brent (fun x -> f x max_y) (fst range_x) (snd range_x)) in
  (max_x, max_y, f max_x max_y)

let max3 f range_x range_y range_z =

  let max_z = fst (Max1D.brent 
                     (fun z -> trd (max2 (fun x y -> f x y z) range_x range_y))
                     (fst range_z) (snd range_z)) in
  let (max_x,max_y,_) = max2 (fun x y -> f x y max_z) range_x range_y in
  (max_x, max_y, max_z, f max_x max_y max_z)

let circ_linear_regress xs phis =

  let score x_hat mu_hat beta_hat = 
    let g x = 2. *. atan (beta_hat *. x) in
    Base.phase_vector_fold_left2
               (fun p x phi -> p +. cos (phi -. mu_hat -. g x)) 0. xs phis
  in
  let bnd_m1, bnd_p1, bnd_b1 = ((-100.), 100.), (Base.npi, Base.pi), ((-20.), 20.)
  and bnd_m2, bnd_p2, bnd_b2 = ((-100.), 100.), (0., Base.pi2), ((-20.), 20.) in
  match (max3 score bnd_m1 bnd_p1 bnd_b1, max3 score bnd_m2 bnd_p2 bnd_b2) with
      ((mu_hat,phi_hat,beta_hat,s1),(_,_,_,s2)) when s1 > s2 -> mu_hat,phi_hat,beta_hat
    | (_, (mu_hat,phi_hat,beta_hat,_)) -> mu_hat, phi_hat, beta_hat

(*
  let xs0 = map ((+.) ((-. 1.) *. mean_x)) xs in
  let phis0 = map ((+.) ((-. 1.) *. mean_p)) phis in

  let maximize_beta x0 = 
    let xs0 = subtract_offset xs x0 in
    (*Max1d.brent returns a small interval containing the max *)
    pair_mean ( Max1D.brent (score xs0 phis0) (-.100.) (100.) ) 
  in

  let maximize_x beta0 =
    pair_mean 
      (Max1D.brent (fun x -> score (add_offset xs x) beta0) (-.100.) (100.))
  in

  let rec iterate_n (x0, beta0) n = match n with
      0 -> (x0,beta0)
    | n -> let beta1 = maximize_beta x0
*)  



















