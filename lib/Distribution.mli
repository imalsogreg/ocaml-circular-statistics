(** {6 Circular Probability Distribution Functions} *)

(** Distribution parameters *)
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




















