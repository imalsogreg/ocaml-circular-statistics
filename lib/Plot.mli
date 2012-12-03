(** Initialize a plotting viewport for plotting on a x/y 1:1 plane *)
val init_circ_plot1 : unit -> Archimedes.Viewport.t
(** [init_circ_plot1 ()] returns an Archimedes viewport handle *)

(** Make an image of a joint pdf using ocaml graphics *)
val image_of_dist2: 
  ?x_range: float * float ->
  ?y_range: float * float ->
  ?size: int * int ->
  ?norm_pdf: bool ->
  ?norm_log_pdf: bool ->
  ?pdf_color: Graphics.color option ->
  ?pdf_log_color: Graphics.color option ->
  Distribution.distribution list -> 
  Graphics.color array array

(* Contour plot options: 
   NoContours or Contours of n_lines, n_line_samples, low_color, high_color *)
type contour_opts = NoContours 
                    | Contours of int * int * Archimedes.Color.t * Archimedes.Color.t

(** Plot a function of two variables (like matlab's imagesc) *)
val arch_2d_fn : 
  Archimedes.Viewport.t ->
  ?domain : ((float * float) * (float * float)) option  ->
  ?subsample : int ->
  ?pt_size : int ->
  ?norm_c : bool ->
  ?c_range : (Archimedes.Color.t * Archimedes.Color.t) option ->
  ?log_c_range : (Archimedes.Color.t * Archimedes.Color.t) option ->
  ?contour : contour_opts option ->
  (float -> float -> float)
(** [arch_2d_fx fig fn] plots colored points on an Archimedes viewport fig
    according to function fn 
    TODO: explain optional arguments*)


    
     



















