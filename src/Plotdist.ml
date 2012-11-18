open Circstat.Base
open Circstat.Circdists


let image_of_dist2 ?(x_range = -. pi, pi) ?(y_range = -. pi , pi ) ?(size = 500,500)
    ?(norm_pdf = true) ?(norm_log_pdf = true) 
    ?(pdf_color = Some of Graphics.blue) ?(pdf_log_color = Graphics.red)
    pdf_list =
  let rec range (start, stop) n =
    let step = (stop -. start) /. (float_of_int (n - 1)) in 
    let aux accum val =
                    if (val > stop) then accum else
                      aux (val::accum) (val +. step)
    in 
    List.rev (aux [] start)
  in 
  let xs,ys = (range x_range (fst size)), (range y_range (snd size)) in 

  let pdf = List.map (List.map (fun fx y -> eval_pdf y::[x]) xs) ys in 
  pdf




















