open Circstat.Distribution

let size = (400,400)
let size_s = " " ^ (string_of_int (snd size)) ^ 
  "x" ^ (string_of_int (fst size))

let iv = LinNormal( Constant (-. 1.), Constant 5.)
let link = fun x -> 2. *. atan x +. 1.
let k_link = fun x -> (2. +. (sin (x *. 4.))) *. 10.
let dv = VonMises( Dependent link, Dependent k_link)

let im = Circstat.Plot.image_of_dist2 
  ~x_range:(-. 3., 3.) 
  ~size:size [dv;iv]
  ~pdf_color:(Some Graphics.white)
  ~pdf_log_color:(Some Graphics.blue)  in
Graphics.open_graph size_s;
Graphics.draw_image (Graphics.make_image im) 0 0

let b = 1










