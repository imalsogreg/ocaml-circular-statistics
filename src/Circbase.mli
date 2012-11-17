(** Circbase 
    Module with utility functions common to many other modules.
    Also useful to library user for generating bins and comparing
    circular values *)

(** Pi and 2Pi *)
val pi : float
val pi2 : float

(** Common vector type*)
type phase_vector = Gsl.Vector.vector
type vec = phase_vector
type mat = Gsl.Matrix.matrix

(** {6 Basic comparison operators} *)

(** (%) + modulo operator *)
val (%) : float -> float -> float
(** Always returns positive remainders. *)

(** Circular less-than *)
val (@<) : float -> float -> bool
(** [a @< b] returns true if a is circularly less than b.
    A phase is 'less' than another if the 
    course from (a % 2pi) to (b % 2pi) going
    counterclockwise is less than pi radians. *)

(** Circular less-than-or-equal-to *)
val (@<=) : float -> float -> bool
(** [a @<= b] Same as circular less-than,
    also true if circularly equal *)

(** Circularly equal *)
val (@=) : float -> float -> bool
(** [a @= b] retuns true if (a % 2pi) = (b % 2pi) *)

    
(** Circular greater-than *)
val (@>) : float -> float -> bool
(** [a @> b] returns true if a is circularly greater than b *)

(** Circular greater-than-or-equal-to *)
val (@>=) : float -> float -> bool
(** [a @>= b] returns true if a is circularly greater than
    or equal to b *)


(** {6 Arithmetic operators} *)

(** Circular difference *)
val (@-) : float -> float -> float
(** [a @- b] returns the shortest phase distance between
    a and b.  Positive if a @> b. In some situations it is 
    more useful when taken as positive-modulo 2pi 
    (e.g. when taking the half-way point between a and b) *)

(** Circular sum *)
val (@+) : float -> float -> float
(** [a @+ b] returns the sum of phases a and b, modulo'd
    into the range [-pi, pi) *)

(** {6 Array creation / manipulation} *)

val circspace : 
  ?offset:float -> 
  ?halfshift:bool-> 
  ?addfinal:bool ->
  ?start:float -> ?stop:float -> 
  int -> phase_vector
(** [circspace ~offset:0. ~halfshift:false ~addfinal: false ~start:0. ~stop:pi2 n] returns
    an array of N floats evenly spaced, beginning at ~start and ending one spacing before
    ~stop (when ~addfinal:false), or at ~stop itself (when ~addfinal:true).  Defaults
    produce coverage of the circle, useful as the abscissa of a rose plot.
    With ~halfshift:true points are shifted forward by one half bin, and become useful
    as bin boundaries for circular histogram.
    With ~halfshift:true and ~addfinal:true, the final value of the array is greater
    than ~stop. *)

val intermediate_points : phase_vector -> phase_vector
(** [intermediate_points phase_array] returns an array of points, spaced evenly between
    array-consecutive pairs of points in the index array.  This is handy for converting
    between bin-centers and bin-edges for circular histograms. *)

val phase_vector_init : int -> (int -> float) -> phase_vector
(** [phase_vector_init n f] creates an n-element vector with elements determined from the indices *)

val phase_vector_fold_left : ('a -> float -> 'a) -> 'a -> phase_vector -> 'a
(** [phase_vector_fold_left f init_val phase_array] *)

val phase_vector_map : (float -> float) -> phase_vector -> phase_vector

val phase_vector_mapi : (int -> float -> float) -> phase_vector -> phase_vector

val phase_vector_iter : (float -> unit) -> phase_vector -> unit

val phase_vector_iteri : (int -> float -> unit) -> phase_vector -> unit

(** {6 Search in sorted lists} *)

(** Circular comparison *)
val ccompare : float -> float -> int
(** Standard compare function for phase variables, used in sorting *)

(** Circular closest-element lookup *)
val binary_closest_ind : phase_vector -> float -> int
(** [binary_closest_ind phase_array key_value] returns the index of the phase array 
    element with angle closest to the key.  ASSUMES phase_array is sorted ascending
    (counterclockwise) *)

(** Circular closest index above *)
val closest_ind_above : phase_vector -> float -> int
(** [closest_ind_above phase_array phase] returns the index of the closest
    array element e that is @> phase.  ASSUMES phase_array is sorted ascending *)

(** Circular closest index above or equal *)
val closest_ind_above_or_equal : phase_vector -> float -> int
(** Same as closest_ind_above, but also accept array value equal to input key
    ASSUMES that phase_array is sorted ascending *)

(** Circluar closest index below *)
val closest_ind_below : phase_vector -> float -> int
(** Same as closest_ind_above, but for array values smaller than the key.
    ASSUMES that phase_array is sorted ascending *)

(** Circular closest index below or equal *)
val closest_ind_below_or_equal : phase_vector -> float -> int
(** Same as closest_ind_above, but for array elements @<= than the key.
    ASSUMES that phase_array is sorted ascending *)

(** {6 Vector operations} **)


(** initialize phase vector **)
val phase_vector_init : int -> (int -> unit) -> unit

(** iterate over phase vector elements **)
val phase_vector_iter : (float -> unit) -> phase_vector -> unit

(** iterate over phase vector indices and elements **)
val phase_vector_iteri : (int -> float -> unit) -> phase_vector -> unit

(** print phase vector elements **)
val phase_vector_print : phase_vector -> unit

(** fold left over phase vector **)
val phase_vector_fold_left : ('a -> float -> 'a) -> 'a -> phase_vector -> 'a

(** phase vector map - but only maps to another phase vector **)
val phase_vector_map : (float -> float) -> phase_vector -> phase_vector

(** phase vector map with indices - but only maps to another phase vector **)
val phase_vector_mapi : (int -> float -> float) -> phase_vector -> phase_vector




















