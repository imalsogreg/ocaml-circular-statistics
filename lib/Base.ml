(* Always-positive mod *)
let (%) a b =
  let c = mod_float a b in 
  if c >= 0. then c else c +. b

(* alias 2pi for convenience *)
let pi = 4. *. atan 1.
let pi2 = 2. *. pi
let npi = (-. 1.) *. pi
let n2pi = (-. 2.) *. pi

module V = Gsl.Vector
module M = Gsl.Matrix

type phase_vector = Gsl.Vector.vector
type vec = Gsl.Vector.vector
type mat = Gsl.Matrix.matrix

(* Circular comparisons
   a < b when linear b is greater
   than linear a, until it becomes
   greater by pi.  When b is ahead
   of a by pi or more, then a is 
   greater *)
let (@<) th_a th_b =
  let th_diff = (th_b -. th_a) % pi2 in
  (th_diff > 0.) & (th_diff < pi)

let (@=) th_a th_b =
  (th_a = th_b) ||
    (th_a -. th_b) % pi2 = 0.

let (@<=) th_a th_b =
  (th_a @< th_b) || 
    (th_a @= th_b)

let (@>) th_a th_b =
  let th_diff = (th_b -. th_a) % pi2 in
  (th_diff > pi) & (th_diff < pi2)

let (@>=) th_a th_b =
  (th_a @> th_b) ||
    (th_a @= th_b)

let (@-) th_a th_b =
  (((th_a -. th_b) +. pi) % pi2) -. pi

let (@+) th_a th_b =
  (((th_a +. th_b) +. pi) % pi2) -. pi

let comparison_test_cases = [
(0., 0., (@=), true);
(1., 1. +. ( 6. *. pi), (@=), true);
(1., 2., (@<=), true);
(1., 2., (@<=), true);
(2., 0.1, (@<=), false);
(0.1, 4., (@<), false);
(5., 0.1, (@>), false);
(3., 5., (@<=), true);
(3., 6.05, (@<=), true);
]

let comparison_test () = 
  let res =
  List.fold_left (&)
    true
    (List.map (fun (a,b,c,r) -> (c a b = r) = true) comparison_test_cases)
  in
  if res then () else
    failwith "Failed circular comparison unit test."

let phase_vector_set_by_fi (f: int -> float) (arr: Gsl.Vector.vector) = 
  for n = 0 to (V.length arr - 1) do
    arr.{n} <- f n
  done

let phase_vector_init n (f : int -> float)  =
  let arr = Gsl.Vector.create n in
  phase_vector_set_by_fi f arr;
  arr
  
let phase_vector_iter f arr =
  for n = 0 to (V.length arr -1 ) do
    f arr.{n}
  done

let phase_vector_iteri f arr =
  for n = 0 to (V.length arr -1 ) do
    f n arr.{n}
  done

let phase_vector_print arr =
  phase_vector_iter (fun x -> Printf.printf "%f " x) arr;
  print_endline ""
    
(* Built this way first, then the following way, to test speed *)
let phase_vector_fold_left_a f i arr =
  let rec aux f subarr accum =
    if V.is_null subarr then accum else
    aux f (V.subvector subarr 1 (V.length subarr - 1)) (f accum subarr.{0})
  in
  aux f arr i

(* This one was a bit faster *)
let phase_vector_fold_left f i arr =
  let a = ref i in
  for n = 0 to (V.length arr - 1) do
    a := f !a arr.{n}
  done;
  !a

let phase_vector_length (arr: phase_vector) = Gsl.Vector.length arr


let phase_vector_fold_left2 f i arr1 arr2 =
  let a = ref i in
  for n = 0 to (phase_vector_length arr1 - 1) do
    a := f !a arr1.{n} arr2.{n}
  done;
  !a

  
let phase_vector_map f arr =
  let a = V.create (V.length arr) in
  for n = 0 to (V.length arr) - 1 do
    a.{n} <- (f arr.{n})
  done;
  a

let phase_vector_mapi f arr =
  let a = V.create (V.length arr) in 
  for n = 0 to (V.length arr) - 1 do
    a.{n} <- (f n arr.{n})
  done;
  a


let array_merge a1 a2 =
  let a = Array.make (min (Array.length a1) (Array.length a2)) (0.,0.) in
  let rec aux n = match n with
      0 -> ()
    | n -> a.(n) <- (a1.(n), a2.(n)); aux (n-1)
  in
  aux (Array.length a - 1);
  a

let phase_vector_merge l1 l2 =
  let l1' = V.to_array l1
  and l2' = V.to_array l2 in
  array_merge l1' l2'



(* throwaway profile function *)
let timer n f arg =
  let t0 = ref (Unix.gettimeofday ()) in
  for i = 1 to n do
    f arg
  done;
  ((Unix.gettimeofday ()) -. !t0) /. (float_of_int n)

let circspace
    ?(offset = 0.) 
    ?(halfshift = false) 
    ?(addfinal = false)
    ?(start = 0.) ?(stop = pi2) n =
  let aux_n = if not addfinal then n else (n-1) in
  let spacing = (stop -. start) /. (float_of_int aux_n) in
  let total_offset = 
    if halfshift then 
      offset +. (spacing /. 2.) else
      offset
  in
  phase_vector_init n (fun i -> 
    (((spacing *. (float_of_int i)) +. start) +. total_offset))



(* Unused? *)
(* Change phase angle limits
   (ie describe phase angle in (-pi,pi) or (0,2pi) *)
let to_range (lower,upper) theta =
  (mod_float (theta -. lower) pi2) +. lower

let closest_of_this_and_neighbors arr ind th =
  let dist a b = abs_float( a @- b) in
  let closest_ind = ref ind in
  
  if (ind > 0) & 
    (dist arr.{ind-1} th) <  (dist arr.{!closest_ind} th)
  then closest_ind := (ind-1) else ();

  if (ind < (V.length arr -1) ) &
    (dist arr.{ind+1} th) < (dist arr.{!closest_ind} th)
  then closest_ind := (ind+1);
  
  !closest_ind

let ccompare a b =
  if a @= b then 0 else
    if a @< b then -1 else 1
    
let binary_closest_ind (arr: phase_vector) th =
  let touchup_ind i = closest_of_this_and_neighbors arr i th in
  let mid_ind a b = (b-a)/2 + a in
  let rec aux min max =
    let test_ind = mid_ind min max in 
    let d = ccompare th arr.{test_ind} in 
    match d with
        0 -> touchup_ind test_ind
      | _ when d < 0 -> if test_ind <> max then aux min test_ind
        else touchup_ind test_ind
      | _ when d > 0 -> if test_ind <> min then aux test_ind max
        else touchup_ind test_ind
      | _ -> failwith "Impossible case"
  in
  aux 0 (V.length arr - 1)
;;

let closest_ind_above arr th =
  (* Assuming arr is sorted ascending *)
  let closest_ind = binary_closest_ind arr th in
  if arr.{closest_ind} @> th then closest_ind else
      let next_ind = if closest_ind = (V.length arr - 1) then 0
        else closest_ind + 1
      in
      if arr.{next_ind} @> th then next_ind
      else raise Not_found
      

let closest_ind_above_or_equal arr th =
  let closest_ind = binary_closest_ind arr th in
  if arr.{closest_ind} = th then closest_ind else
    closest_ind_above arr th


let closest_ind_below arr th =
  (* Assuming arr is sorted ascending *)
  let closest_ind = binary_closest_ind arr th in
  if arr.{closest_ind} @< th then closest_ind else
    let  next_ind = if closest_ind = 0 then V.length arr -1
      else closest_ind - 1
    in
      if arr.{next_ind} @< th then next_ind
      else raise Not_found

let closest_ind_below_or_equal arr th =
  let closest_ind = binary_closest_ind arr th in
  if arr.{closest_ind} = th then closest_ind else
    closest_ind_below arr th


let intermediate_points arr =
  (* Add first element of arr again at the end, because we want
     to find the point between arr(last) and arr(first), too *)
  let arr_pad =  V.create (V.length arr + 1) in
  arr_pad.{V.length arr_pad - 1} <- arr_pad.{0};
  (* We're following the circle counterclockwise, so
     all incremental steps have to be in [0,pi2], hence
     the mod by pi2 (-. and @- often give negative diffs) *)
  let dist_to_next i = (arr_pad.{i+1} -. arr_pad.{i}) % pi2 in
  phase_vector_mapi (fun i x -> x +. (dist_to_next i) /. 2.) arr



let test_a = circspace ~start:(pi *. 3. /. 2.) ~stop: (pi /. 2.) 100
let test_b = circspace ~start:0. ~stop:pi 10001




















