
module D = Circstat.Distribution

let size = (300,300)
let size_s = " " ^ (string_of_int (snd size)) ^ 
  "x" ^ (string_of_int (fst size))

let rec demo_vm_by_lin beta =

  let iv = D.LinNormal( D.Constant (-. 1.), D.Constant 5.) in
  let link = fun x -> 2. *. atan (beta *.x) in
  let k_link = fun x -> (2. +. (sin (x *. 4.))) *. 10. in
  let dv = D.VonMises( D.Dependent link, D.Dependent k_link) in

  let im = Circstat.Plot.image_of_dist2 
    ~x_range:(-. 3., 3.) 
    ~size:size [dv;iv]
    ~pdf_color:(Some Graphics.white)
    ~pdf_log_color:(Some Graphics.blue)  in
  
  Graphics.draw_image (Graphics.make_image im) 0 0;
  let status = Graphics.wait_next_event [Graphics.Key_pressed] in
  match status with
      {Graphics.key = 'q'} -> ()
    | {Graphics.key = 'b'} -> demo_vm_by_lin (beta -. 0.1)
    | {Graphics.key = 'B'} -> demo_vm_by_lin (beta +. 0.1)
    | _ -> 
        print_endline "Unrecognized input.  Use b, B, or q";
        demo_vm_by_lin beta
      

let _ =
  Graphics.open_graph size_s;
  demo_vm_by_lin 2.












