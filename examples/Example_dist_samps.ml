(*module C = Circstat
module D = C.Distribution
module S = C.Sample
module P = Circstat.Plot
module A = Archimedes *)

let linkfn x = 2. *. atan x
let example_dist = 
  [Distribution.VonMises(Distribution.Dependent linkfn, Distribution.Constant 1.); 
   Distribution.LinUniform(Distribution.Constant (-.4.), Distribution.Constant 4.)]
let example_f x y = Distribution.eval_pdf example_dist [y;x]


let _ =
  let vp = Plot.init_circ_plot1 ~mode:["graphics";"hold"] () in
  Plot.arch_2d_fn vp ~domain:(Some(((-. 4.),4.),((-4.),4.)))example_f;
  let (xs,ys) = Sample.xs_ys_of_n_samples example_dist 10 in
  Plot.arch_2d_samps vp xs (List.map (Base.to_range (Base.npi, Base.pi)) ys);
  Archimedes.show vp;
  Archimedes.close vp




















