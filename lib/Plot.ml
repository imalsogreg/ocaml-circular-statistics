let (pi,pi2) = Base.(pi,pi2)
let eval_pdf,grid_eval_pdf = Distribution.(eval_pdf, grid_eval_pdf)
module A = Archimedes
module AV = A.Viewport
module G = Graphics


(* which one to use?  sum_with_max, or clip_to? *)
let sum_with_max m a b =
  let s = a+.b in
  if s < m then s else m
let clip_to m input =
  if input > m then m else input

let array_max a =
  Array.fold_left max neg_infinity a
let array_min a =
  Array.fold_left min infinity a

let matrix_max m =
  array_max (Array.map array_max m)
let matrix_min m =
  array_min (Array.map array_min m)

let array_map2 f a b =
  if Array.length a <> Array.length b then failwith "array_map2 mismatched input sizes."
  else Array.init (Array.length a) (fun i -> f a.(i) b.(i))

let matrix_add m1 m2 =
  array_map2 (array_map2 (sum_with_max 255.)) m1 m2

let matrix_iter f m =
  Array.map (Array.iter f) m

let matrix_map f m =
  Array.map (Array.map f) m

let array_reverse a =
  let b = Array.copy a in
  for m = 0 to Array.length b / 2 - 1 do
    let tmp = b.(m) in
    b.(m) <- b.(Array.length b - m - 1);
    b.(Array.length b - m - 1) <- tmp
  done;
  b


let get_rgb c = (c/65536, (c/256) mod 256, c mod 256)

let cmap (target_c: Graphics.color) v =
  let (target_r, target_g, target_b) = get_rgb target_c in
  Graphics.rgb
    (int_of_float (float_of_int target_r *. v))
    (int_of_float (float_of_int target_g *. v))
    (int_of_float (float_of_int target_b *. v))


let color_add a b =
  let c_add a b = let c = a + b in if c < 255 then c else 255 in
  let (ra, ga, ba),(rb, gb, bb) = get_rgb a, get_rgb b in
  Graphics.rgb (c_add ra rb) (c_add ga gb) (c_add ba bb)

let image_add a b =
  array_map2 (array_map2 color_add) a b

let matrix_normalize m =
  let (m_min, m_max) = matrix_min m, matrix_max m in
  let update_value x = (x -. m_min) /. (m_max -. m_min) in
  matrix_map update_value m

let image_of_dist2
    ?(x_range = (-. pi, pi) )
    ?(y_range = (-. pi , pi) ) ?(size = 500,500)
    ?(norm_pdf = true) ?(norm_log_pdf = true)
    ?(pdf_color = Some Graphics.blue) ?(pdf_log_color = Some Graphics.red)
    pdf_list =
  let eval_points range n =
    Base.circspace ~start:(fst range) ~stop:(snd range) n in
  (*
    let xs,ys = Gsl.Vector.to_array(eval_points x_range (fst size)),
    array_reverse (Gsl.Vector.to_array (eval_points y_range (snd size)))  in
  *)
  let xs,ys = (eval_points x_range (fst size)),
    Gsl.Vector.of_array (array_reverse (Gsl.Vector.to_array (eval_points y_range (snd size))))  in

  let pdf = grid_eval_pdf pdf_list xs ys in
  let log_pdf = Array.map (Array.map log) pdf in
  let pdf_norm = if norm_pdf
    then matrix_normalize pdf
    else pdf in
  let pdf_log_norm = if norm_log_pdf
    then matrix_normalize log_pdf
    else log_pdf in
  let base_image = Array.make_matrix (snd size) (fst size) Graphics.black in
  let new_base = match pdf_color with
    | None -> base_image
    | Some target_color -> let pdf_image = matrix_map (cmap target_color) pdf_norm in
                           image_add base_image pdf_image
  in
  match pdf_log_color with
    | None -> new_base
    | Some target_color ->
        let norm_pdf_image = matrix_map (cmap target_color) pdf_log_norm in
        image_add new_base norm_pdf_image


let get_xlim vp =
  (A.Viewport.xmin vp, A.Viewport.xmax vp)

let set_xlim vp range =
  A.Viewport.xrange vp (fst range) (snd range)

let get_ylim vp =
  (A.Viewport.ymin vp, A.Viewport.ymax vp)

let set_ylim vp range =
  A.Viewport.yrange vp (fst range) (snd range)

(* let init_plot1 ?(size = 500,400) ?(complex_plane = true) max_value:float = *)
let init_circ_plot1 
    ?(mode = ["graphics";"hold"])
    () =
  let module A = Archimedes in
  let vp = A.init mode in
  A.Viewport.axes_ratio vp 1.;
  A.Axes.box vp;
  vp

let center vp =
  let w,h = (A.Viewport.xmax vp -. A.Viewport.xmin vp), (A.Viewport.ymax vp -. A.Viewport.ymin vp) in
  A.Viewport.xrange vp (0. -. w /. 2.) (w /. 2.);
  A.Viewport.yrange vp (0. -. h /. 2.) (h /. 2.);
  ()

let center_and_fit_circle vp r =
  center vp;
  let x_scale = max (r /. A.Viewport.xmax vp) 1. in
  let y_scale = max (r /. A.Viewport.ymax vp) 1. in
  let scale = max x_scale y_scale in
  A.Viewport.xrange vp (A.Viewport.xmin vp *. scale) (A.Viewport.xmax vp *. scale);
  A.Viewport.yrange vp (A.Viewport.ymin vp *. scale) (A.Viewport.ymax vp *. scale);
  ()


let plot_circ_dist1 figure ?(r_0 = 10.) ?(r_scale = 5.) f =
  let r_of_p p = r_0 +. r_scale *. p in
  let dist_f (d: float -> float) theta = let rad = r_of_p (d theta) in
                                         (rad *. cos theta, rad *. sin theta)
  in
  A.xyf ~fill:true ~fillcolor:A.Color.cyan figure (dist_f f) 0. pi2;
  A.xyf ~fill:true ~fillcolor:A.Color.white figure (function t -> r_0 *. cos t, r_0 *. sin t) 0. pi2;
  ()
(*  let dimx,dimy = A.Viewport.dimensions figure in
    let xrange,yrange = get_xlim vp, get_ylim vp in
    let n_ts = (dimx *. r_0) /. snd xrange in*)

let plot_phase_vector figure ?(r_0 = 10.) ?(r_1 = 11.) v =
  let mk_tic p = let xs,ys =
                   [r_0 *. cos p; r_1 *. cos p], [r_0 *. sin p; r_1 *. sin p]
                 in  A.List.xy figure ~style:`Lines xs ys
  in
  Base.phase_vector_iter mk_tic v


let b = 1

type contour_opts = int * int * Archimedes.Color.t * Archimedes.Color.t
let contour_default = 10, 100, Archimedes.Color.black, Archimedes.Color.white


let color_from_range c_low c_high v_low v_high x =
  let pref_val = (x -. v_low)  /. (v_high -. v_low) in
  let interp l h = l +. pref_val *. (h -. l) in
  let r, g, b, a = Archimedes.Color.get_rgba c_low in
  let r',g',b',a' = Archimedes.Color.get_rgba c_high in
  Archimedes.Color.rgba (interp r r') (interp g g') (interp b b') (interp a a')

let rec drop_n l n =
  match l with
      xs when n <= 0 -> xs
    | x::xs -> drop_n xs (n-1)
    | [] -> []

let rec drop_until pred l =
  match l with
      hd::tl when not (pred hd) -> drop_until pred tl
    | x -> x

let rec l_last l =
  match l with
      [] -> []
    | x::[] -> x
    | x::xs -> l_last xs

let px_in_domain d_start d_stop px subsample =
  let border_dist = match px with p0::p1::_ -> (p1 -. p0) /. 2. | _ -> 0. in
  let p_in_domain p = p >= d_start -. border_dist && p <= d_stop +. border_dist in
  let rec aux p acc =
    match p with
        p0::[] when p_in_domain p0 -> List.rev (p0::acc)
      | p0::[]                     -> List.rev (acc)
      | p0::tl when p_in_domain p0 -> let n_to_drop = (min (List.length tl) subsample) - 1 in
                                      aux (drop_n tl n_to_drop) (p0::acc)
      | p0::_ -> List.rev acc
      | [] -> failwith "Impossible case"
  in
  aux (drop_until p_in_domain px) []

let rec list_init f n =
  let rec aux i acc = if (i = n-1)
    then List.rev acc
    else aux (i+1) (f i :: acc)
  in aux 0 []


let fig_xs_ys fig =
  let module A = Archimedes.Viewport in
  let x0,x1 = A.xmin fig, A.xmax fig in
  let y0,y1 = A.ymin fig, A.ymax fig in
  let xn,yn  = A.dimensions fig in
  let dx,dy = (x1 -. x0) /. (xn -. 1.), (y1 -. y0) /. (yn -. 1.) in
  let xs = list_init (function i -> x0 +. (float_of_int i) *. dx) (int_of_float xn)
  and ys = list_init (function i -> y0 +. (float_of_int i) *. dy) (int_of_float yn)
  in
  xs, ys

let rec final_tail ls = 
  match ls with 
      [] -> failwith "tail of empty list"
    | hd::[] -> hd 
    | hd::tl -> final_tail tl

let keep_n l n =
  let rec aux ls n_more acc =
    match ls with
        [] -> List.rev acc
      | _ when n_more = 0 -> List.rev acc
      | hd::tl -> aux tl (n_more - 1) (hd::acc) 
  in aux l n []

let take_every_nth l n =
  let rec aux ls acc = match ls with 
      [] -> List.rev acc 
    | hd::tl -> aux (drop_n tl (n-1)) (hd::acc)
  in aux l []

(*
let take_every_nth_with_caps ls subsample =
  match ls with
    | []
    | _::[] 
    | _::_::[] -> ls
    | hd::tl -> 
        let headcap, middle, tail = hd, (keep_n tl (List.length tl - 1)), final_tail ls
        in
        let rec aux l acc = match l with
            [] -> List.rev (tail::acc)
          | hd::tl -> aux (drop_n l subsample) (hd::acc)
        in headcap::(aux middle []) 
        *)

let plot_xs_ys fig domain subsample =
  let ((xmin,xmax),(ymin,ymax)) = domain in
  let xs,ys = fig_xs_ys fig in
  let bx,by =
    (List.nth xs 2 -. (List.nth xs 1)) /. 2.,
    (List.nth ys 2 -. (List.nth ys 1)) /. 2. in
  let filter_and_stay_in_bounds ps bmin bmax =
    List.map (fun p -> match p with
        x when x < bmin -> bmin
      | x when x > bmax -> bmax
      | x -> x)
      (List.filter (fun x -> x >= bmin && x <= bmax) ps)
  in
  let ok_xs = 
    take_every_nth (filter_and_stay_in_bounds xs (xmin -. bx) (xmax +. bx)) subsample
  and ok_ys = 
    take_every_nth (filter_and_stay_in_bounds ys (ymin -. by) (ymax +. by)) subsample
  in ok_xs, ok_ys

let list_min l = 
  List.fold_left min infinity l
let list_max l =
  List.fold_left max neg_infinity l

let comp_one_val (f: float -> float -> float) (x: float) (y: float) =
  x,y, f x y

let arch_add_square 
    ?(stroke_fun = fun (r,g,b,a) -> (r/.2.,g/.2.,b/.2.,a/.2.) )
    ?(fill_fun = fun (r,g,b,a) -> (r,g,b,a) )
    s vp
    =
  let module B = Archimedes.Backend in
  let marker_name = "S" ^ string_of_int s in
  let f = float_of_int s in
  Archimedes.Marker.add marker_name (fun handle ->
    let context_color = Archimedes.Viewport.get_color vp in
    let r', g', b', a' = fill_fun (Archimedes.Color.get_rgba context_color) in
    let fill_c = Archimedes.Color.rgba r' g' b' a' in
    let r'', g'', b'', a'' = stroke_fun (Archimedes.Color.get_rgba context_color) in
    let stroke_c = Archimedes.Color.rgba r'' g'' b'' a'' in

    B.rectangle handle ~x:0. ~y:0. ~w:f ~h:f;
    Archimedes.Backend.set_color handle fill_c;
    B.fill handle;

    B.rectangle handle ~x:0. ~y:0. ~w:f ~h:f;
    Archimedes.Backend.set_color handle stroke_c;
    B.stroke handle; 

    Archimedes.Backend.set_color handle context_color
  )
    {Archimedes.Matrix.x=((-. f) /. 2.); y=((-. f) /. 2.); w=f; h=f};
  marker_name
    

let arch_2d_fn 
    (fig: Archimedes.Viewport.t)
    ?(domain = Some((-. 1., 1.), (-1., 1.)))
    ?(subsample = 20)
    ?(pt_size = 1) (* I haven't figured out how to make bigger markers in archimedes yet *)
    ?(norm_c = true)
    ?(c_range = Some(Archimedes.Color.rgba 0. 0. 0. 0.5, Archimedes.Color.white))
    ?(log_c_range = None)  (* A bug somewhere causes use of log_c_range to produce out-of-bounds colors  *)
    ?(contour : contour_opts option = None)
    ?(grow_plot = true) (* Without grow_plot, if this is the first thing being plotted, what do we do? Is a fresh plot initialized to some size? *)
    (f: float -> float -> float) =
  let module A = Archimedes in
  let module AV = A.Viewport in
  let x_domain, y_domain =
    match domain with
        Some (((xmin,xmax),(ymin,ymax)) as d)  when grow_plot = true -> d
      | Some ((xmin,xmax) , (ymin,ymax) ) ->
          ((max xmin (AV.xmin fig), min xmax (AV.xmax fig)),
           (max ymin (AV.ymin fig), min ymax (AV.ymax fig)))
      | None -> (AV.xmin fig, AV.xmax fig), (AV.ymin fig, AV.ymax fig)
  in
  A.Viewport.auto_fit fig (fst x_domain) (fst y_domain) (snd x_domain) (snd y_domain);
  let xs,ys = plot_xs_ys fig (x_domain,y_domain) subsample in
  let (pts_vals: (float * float * float) list list ) = 
    List.map (fun y -> (List.map (fun x -> comp_one_val f x y) xs) ) ys in
  let vmin = list_min (List.map (List.fold_left (fun i (x,y,p) -> min i p) infinity) pts_vals)
  and vmax = list_max (List.map (List.fold_left (fun i (x,y,p) -> max i p) neg_infinity) pts_vals) in
  let l_vmin,l_vmax = log vmin, log vmax in

  let our_marker = arch_add_square (subsample / 8) fig in  (* This is defined so late b/c it depends on the viewport being sized.  Ugly? *)

  let render_point (x,y,p) = 
    let (c: Archimedes.Color.t) = match c_range with 
        Some (c_low, c_high) -> color_from_range c_low c_high vmin vmax p
      | None -> Archimedes.Color.rgba 0. 0. 0. 0.
    and (l_c: Archimedes.Color.t) = match log_c_range with
        Some (c_low, c_high) -> color_from_range c_low c_high l_vmin l_vmax p
      | None -> Archimedes.Color.rgba 0. 0. 0. 0. in
    A.set_color fig (Archimedes.Color.add l_c c ~op:Archimedes.Color.Over) ;
    A.Array.xy fig ~style:(`Markers our_marker) [|x|] [|y|] 
  in
  List.iter (List.iter render_point) pts_vals
    

let arch_2d_samps fig xs ys =
  A.set_color fig Archimedes.Color.red;
  Archimedes.List.xy fig xs ys

















