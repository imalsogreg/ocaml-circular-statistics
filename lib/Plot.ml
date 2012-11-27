let pi = Base.pi
let eval_pdf,grid_eval_pdf = Distribution.(eval_pdf, grid_eval_pdf)

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

             
(* let init_plot1 ?(size = 500,400) ?(complex_plane = true) max_value:float = *)
let init_plot1 () = 
  let module A = Archimedes in
  let vp = A.init ["graphics";"hold"] in
  A.Viewport.axes_ratio vp 1.;
  A.Axes.box vp;
  vp
    
let fit_rect 

let plot_circ_dist1 figure ?(r_0 = 10.) ?(r_1 = 20.) f = 
  let module A = Archimedes in
  let dimx,dimy = A.Viewport.dimensions figure in
  let xrange,yrange = (A.Viewport.xmin figure, A.Viewport.xmax figure),
    (A.Viewport.ymin figure, A.Viewport.xmax figure) in
  let n_xs = 

    
let b = 1



















