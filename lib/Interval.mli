(** Circinterval - intervals for phase variables *)

(** Type of an interval limit, and an interval *)
type limit_style = Open | Closed
type circ_limit = { lim: float;
                    style: limit_style }

type circ_interval = {t0 : circ_limit; t1 : circ_limit }

(** Create an interval *)
val mk_interval : float -> limit_style -> float -> limit_style -> circ_interval
(** [mk_interval lim0 style0 lim1 style1] returns an interval from lim0 to lim1,
    with each limit being Open or Closed according to style0, style1.
    The 'inside' of the interval is the set of phases traced from lim0 counterclockwise
    to lim1 *)

(** Print interval *)
val string_of_interval : circ_interval -> string

(** Negate interval *)
val negate_interval : circ_interval -> circ_interval
(** [negate_interval intr] returns an interval covering all phases not
    covered by int *)

(** Reverse interval keep boundary style *)
val rev_interval : circ_interval -> circ_interval
(** [rev_interval intr] reverses the limits of the interval intr,
    but keeps the original boundary styles *)

(** {6 Searching in intervals} *)

(** Phase in interval *)
val in_interval : circ_interval -> float -> bool
(** [in_interval intr p] returns true if p is in intr (the set
    of phases between intr's t0 and t1 tracing counterclockwise *)

(** Count elements from phase array in interval *)
val n_in_interval : ?sorted:bool -> Base.phase_vector -> circ_interval -> int
(** [n_in_interval ~sorted:true/false phase_array intr] returns the number
    of elements from phase_array in intr.  Inticate if phase_array
    is sorted ascending to get faster result. *)






















