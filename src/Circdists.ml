open Circstat.Base

let inv_pi2 = 1. /. pi2
let inv_sqrt_pi2 = 1. /. sqrt(pi2)

let vm_pdf mu kappa t=
  (exp (kappa *. cos (t -. mu))) /. (pi2 *. (Gsl.Sf.bessel_I0 kappa))

let cardioid_pdf mu rho t =
  if rho < 0. || rho > 0.5 then failwith "Cardioid rho parameter must be in [0,1/2]" 
  else
    inv_pi2 *. (1. +. 2. *. rho *. cos (t @- mu))

let wrapped_cauchy_pdf mu rho t =
  if rho < 0. || rho > 1. then failwith "Wrapped cauchy rho must be in [0, 1]"
  else
    inv_pi2 *. (1. -. rho *. rho) /. 
      (1. +. rho *. rho -. (2. *. rho *. (cos(t @- mu))))

let lin_uniform_pdf bg en t = if t >= bg && t <= en then 1. /. (en -. bg) else 0.

let lin_normal_pdf mu sigma_sq t =
  let sigma = sqrt sigma_sq in
  1. *. inv_sqrt_pi2 /. sigma *. 
    exp ( (-. 1.) *. ((t -. mu) ** 2.) /. (sqrt( 2. *. sigma *. sigma)) )

type dist_param =
    Constant of float
  | Dependent of (float -> float)

type distribution = 
    VonMises of (dist_param * dist_param)
  | WrappedNormal of (dist_param * dist_param)
  | WrappedCauchy of (dist_param * dist_param)
  | Cardioid of (dist_param * dist_param)
  | CircUniform
  | LinNormal of (dist_param * dist_param)
  | LinUniform of (dist_param * dist_param)

(* P(Y|X=x) = N( u=(mx+b), ssq) *)
(* P(Y=y, X=x) = P(Y|X=x)*P(X=x) *)
(* P(Z=z, Y=y, X=x) = P(Z=z | Y=y, X=x)*P(Y=y, X=x) *)

let rec eval_pdf (dists: distribution list) (xs: float list) =
  match List.combine dists xs with
    | (dv_dist,dv)::tl ->
        begin        
          let get_param = function
          Constant c -> c
            | Dependent f -> 
                (match xs with 
                    hd::iv::tl -> f iv 
                  | hd::[] -> failwith ("Requested deeper parameter from" ^
                      " deepest distribution.")
                )
          in 
          let p_dv_given_iv =
          match dv_dist with
              VonMises (mu, kappa) -> 
                vm_pdf (get_param mu) (get_param kappa) dv
            | CircUniform -> inv_pi2
            | WrappedNormal (mu, rho) -> 
                failwith "Wrapped normal distribution not implemented"
            | WrappedCauchy (mu, rho) -> 
                wrapped_cauchy_pdf (get_param mu) (get_param rho) dv
            | Cardioid (mu, rho) -> 
                cardioid_pdf (get_param mu) (get_param rho) dv
            | LinUniform (bg, en) -> 
                lin_uniform_pdf (get_param bg) (get_param en) dv
            | LinNormal (mu, sigma_sq) ->
                lin_normal_pdf (get_param mu) (get_param sigma_sq) dv
          in 
          let p_iv = eval_pdf (List.tl dists) (List.tl xs) in 
          p_dv_given_iv *. p_iv
        end
    |  _ -> 1.


let pdf_grid (xs: float list list) = 
  List.map 


let d = 1


(*

(* TODO: Get cdf's for these distributions, fit the distributions
  to data (or should this be in another module?) *)

(* What about conditional RV's? *)

let reference_cdf_VM = ref (None : vec option)

module Cdf = struct
  type t = { dist:distribution ;
             theta: float array ;
             pdf: (float -> float);
             cdf: float array }
  let equal a b = 
    a.distribution = b.distribution && 
    a.params = b.params
  let hash a =
    Hashtbl.hash (a.distribution, a.params)
end

module  Cdf_VM = struct
  type key_t = {kappa: float ; precesion:float}
  type t = {key:key_t; xs: vec; data:vec}
  let create kappa precision =
    let x = circspace ~start:(0. -. pi) ~stop:pi (pi2 /. precision) in
    phase_vector_map 
  let equal a b = (a.key = b.key)
  let hash t = Hashtbl.hash t.key
end

module Ctable = Weak.Make (Cdf)


module Cdf = struct
  type t = (float * float * float)
  let equal = (=)
  let hash t = Hashtbl.hash t
end

module type M = (Cdf : Hashdable.HashedType)

module Cdf_VM_hash = Weak.Make (Cdf : Hashtbl.HashedType)

let cdf_VM ?(precision = 0.01) mu kappa 

(* Throw-away timing function *)
let time n f a =
  let t0 = ref (Unix.gettimeofday () ) in
  for n = 1 to n do
    f a
  done;
  (Unix.gettimeofday () -. !t0) /. (float_of_int) n




  *)










