(*
(** {6 Circular Probability Distribution Functions} *)

(** von Misis distribution *)
val vm_pdf : float -> float -> float -> float
(** [vm_pdf mu kappa x] returns the value of VM(mu,kappa) at phase x *)

(** Cardioid distribution *)
val cardioid_pdf : float -> float -> float -> float
(** [cardioid_pdf mu rho x] returns value of cardioid pdf at phase x *)

(** Wrapped cauchy distribution *)
val wrapped_cauchy_pdf : float -> float -> float -> float
(** [wrapped_cauchy_pdf mu rho x] -> WCD(mu,rho) at phase x *)

(** Linear Uniform Distibution *)
val lin_uniform_pdf : float -> float -> float -> float
(** [lin_uniform_pdf beginning ending x] returns the value of the uniform distribution at x *)
(** Provided here for convenience - although obviously not a circular distribution,
    useful to have this for mixed circular-linear joint distributions. *)

(** Circular Uniform Distribution *)
val circ_uniform_pdf : float
(** [circ_uniform_pdf] returns 1/(2 pi), the uniform circular distribution with support
    for all phases. *)
(** TODO: Allow circ_uniform_pdf to take beginning and ending parameters, as the
    lin_uniform_pdf can *)

(** Linear Normal Distribution *)
val lin_normal_pdf : float -> float -> float -> float
(** [lin_normal_pdf mu sig_sq x] returns the pdf value of N(mu,sig_sq) at value x
    Not a circular distribution, but useful for circular-linear joint distributions *)

(** {6 Higher-order distributions} *)

(** Distribution parameters *)
(** TODO: replace variant with a function : float list -> float.  This covers
    the 'constant' case simply eg as: function _ -> 1.4 *)
type dist_param =
  | Constant of float
  | Dependent of (float -> float)
(** Parameters for PDF's are either constant, or dependent
    on the value of an 'independent' RV, by a linking function *)

(** 1-D Distribution type*)
type distribution =
    VonMises of (dist_param * dist_param)
  | WrappedNormal of (dist_param * dist_param)
  | WrappedCauchy of (dist_param * dist_param)
  | Cardioid of (dist_param * dist_param)
  | CircUniform
  | LinNormal of (dist_param * dist_param)
  | LinUniform of (dist_param * dist_param)
(** VonMises, WrappedNormal, Cardiod, and CircUniform are 
    distributions of circular random variables, with their
    parameters (either mu * sigma^2 or mu * kappa)
    LinNarmal and LinUniform are linear RV distributions
    included here for convenient mixing of circular and linear
    distributions *)


(** Full distributions are made by composing 1-D distributions
    Examples:

    *X distributed as VonMises(mu,kap)
           let x_d = [VonMises( Constant mu, Constant kap )]

    *X,Y a correlated linear gaussian pair with Y's mu = mX+b
           let x_d = LinNormal( Constant x1, Constant s1 ) in
           let y_d = LinNormal( Dependent (fun x -> m *. x +. b), Constant s2 ) in
           [y_d,x_d]
    
    *Circ-linear correlation
           let x_d = LinNormal( Constant x1, Constant s1 ) in
           let linker = fun x -> 2. atan x in
           let y_d = VonMises( Dependent linker, Constant kap ) in
           [y_d; x_d]

    More 'dependent' RV's come earlier in the list.  Final RV is the
    independent one.
*)


(** Evaluate PDF *)
val eval_pdf : distribution list -> float list -> float
(** Takes a list of n 1D distribions (dependent first, independent last)
    and a position in R^n, evaluates the PDF at that point *)

(** Evaluate 2D PDF at many points in a grid *)
val grid_eval_pdf : distribution list -> 
  Base.phase_vector -> Base.phase_vector -> float array array
(** Uses eval_pdf on a distribution list, along with a phase_vector
    of x locations and a phase_vector of y locations, returning
    a float array array of the PDF evaluated at all combinations of
    x and y *)




















*)
