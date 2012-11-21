open Circstat.Dist

let iv = LinNormal( Constant 0., Constant 1.)
let link = fun x -> 2. *. atan x 
let dv = VonMises( Dependent link, Constant 1.)

let im = image_of_dist2 [dv;iv]










