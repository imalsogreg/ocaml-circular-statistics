open Base

type limit_style = Open | Closed
type circ_limit = { lim: float;
                    style: limit_style }

(* Interval interior is always the space traced
   from t0 to t1, counterclockwise *)
type circ_interval = {t0 : circ_limit; t1 : circ_limit}

let mk_interval a_v a_s b_v b_s =
  { t0= {lim=a_v; style=a_s};
    t1= {lim=b_v; style=b_s} }

let string_of_interval i =
  let c = function Open -> "()" | Closed -> "[]" in
  let (c0,c1) = (c i.t0.style, c i.t1.style) in
  Printf.sprintf "%c%f, %f%c" (String.get c0 0) i.t0.lim i.t1.lim (String.get c1 1)

let rev_interval intr =
  mk_interval intr.t1.lim intr.t1.style intr.t0.lim intr.t0.style

let negate_interval intr =
  let swap_style = function Open -> Closed | Closed -> Open in
  mk_interval intr.t1.lim (swap_style intr.t1.style)
    intr.t0.lim (swap_style intr.t0.style)


let in_interval interval th =
  let t0_cmp = match interval.t0.style with Open -> (>=) | Closed -> (>)
  and t1_cmp = match interval.t1.style with Open -> (<=) | Closed -> (<)
  in
  let t0_ok = t0_cmp (th % pi2) (interval.t0.lim % pi2)
  and t1_ok = t1_cmp ((th -. interval.t0.lim) % pi2)
    ( (interval.t1.lim -. interval.t0.lim) % pi2) in
  (t0_ok & t1_ok) || (interval.t1.lim -. interval.t0.lim) = pi2



(* Predefined phase angle ranges *)
let z_to_2pi = mk_interval 0. Open pi2 Open
let z_to_pi = mk_interval 0. Open pi Open
let neg_pi_to_z = mk_interval (-. pi) Open 0. Open
let neg_pi_to_pi = mk_interval (-. pi) Open pi Open


let n_in_ind_range first last total =
  if last >= first then (last - first + 1) else
    (* (total- first) + (last + 1) =    *)
    (total - first + last + 1)



let rec n_in_interval ?(sorted=false) arr interval =
  if (not sorted) then
    (* Hand-count *)
    begin
      int_of_float(
      phase_vector_fold_left (+.) 0.
        (phase_vector_map (fun x -> if (in_interval interval x) then 1. else 0.) arr))
    end
  else
    begin
      let cmp0 = match interval.t0.style with
          Open -> closest_ind_above
        | Closed -> closest_ind_above_or_equal
      and cmp1 = match interval.t1.style with
          Open -> closest_ind_below
        | Closed -> closest_ind_below_or_equal in


      try
        let ind0 = cmp0 arr interval.t0.lim
        and ind1 = cmp1 arr interval.t1.lim in
        n_in_ind_range ind0 ind1 (Gsl.Vector.length arr)
      with
          Not_found ->
          (* Bad interval, possibly because start is too far from
             first point (causing first point to look _less_ than interval start)
             or because end is too far from last point, causing end to look like
             it's less than the last point *)
          (* Try to search again, after drawing the interval limits closer
             to the sample points.  As long as we only shrink the interval,
             and don't let the interval cross over any points, the n_int_interval
             answer doesn't change *)
          (* Assuming that arr is sorted ascending, and in [0,pi2) *)
          (* Actually, I don't know who will call this: only know
             that some callers pre-sort.  So instead, we'll just
             iterate over the whole array *)
            Printf.printf "Fell back to hand-counting\n";
            n_in_interval ~sorted:false arr interval
    end



















