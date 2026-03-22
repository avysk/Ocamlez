(*
 * Lorenz attractor:
 *
 * dx/dt = s(y -x)
 * dy/dt = x(r - z) - y
 * dz/dt = xy - bz
 *
 * s - Prandtl number
 * r - Rayleigh number
 *)

(* Default parameter values *)
let default_s = 10.0
let default_b = 8.0 /. 3.0
let default_r = 28.0
let default_size = 500
let default_scale = 5.0
let default_generations = 100000
let default_dt = 0.001

(* Mutable parameters (set from command line) *)
let s = ref default_s
let b = ref default_b
let r = ref default_r
let size = ref default_size
let scale = ref default_scale
let generations = ref default_generations
let dt = ref default_dt
let picture_file = ref None

let parse_float set_ref value =
  try set_ref (float_of_string value)
  with Failure _ ->
    Printf.eprintf "Invalid float value: %s\n" value;
    exit 1

let parse_int set_ref value =
  try set_ref (int_of_string value)
  with Failure _ ->
    Printf.eprintf "Invalid integer value: %s\n" value;
    exit 1

let set_picture filename =
  picture_file := Some filename

let speclist = [
  ("-s", Arg.String (parse_float (fun v -> s := v)), "<float> Prandtl number (default: 10.0)");
  ("--prandtl", Arg.String (parse_float (fun v -> s := v)), "<float> Same as -s");
  ("-b", Arg.String (parse_float (fun v -> b := v)), "<float> Geometric factor (default: 2.67)");
  ("--geometric", Arg.String (parse_float (fun v -> b := v)), "<float> Same as -b");
  ("-r", Arg.String (parse_float (fun v -> r := v)), "<float> Rayleigh number (default: 28.0)");
  ("--rayleigh", Arg.String (parse_float (fun v -> r := v)), "<float> Same as -r");
  ("--size", Arg.String (parse_int (fun v -> size := v)), "<int> Window size in pixels (default: 500)");
  ("--scale", Arg.String (parse_float (fun v -> scale := v)), "<float> Rendering scale factor (default: 5.0)");
  ("-g", Arg.String (parse_int (fun v -> generations := v)), "<int> Number of iterations (default: 100000)");
  ("--generations", Arg.String (parse_int (fun v -> generations := v)), "<int> Same as -g");
  ("--dt", Arg.String (parse_float (fun v -> dt := v)), "<float> Time step (default: 0.001)");
  ("-p", Arg.String set_picture, "<file> Save image to FILE on exit (PPM format)");
  ("--picture", Arg.String set_picture, "<file> Same as -p");
]

let usage = "ocamlez [options]\nOptions:"

let () = Arg.parse (Arg.align speclist) (fun _ -> ()) usage

let ( *.* ) point dt_val =
  let x, y, z = point in
    (x *. dt_val, y *. dt_val, z *. dt_val)

let ( +.+ ) point point' =
  let x, y, z = point in
  let x', y', z' = point' in
    (x +. x', y +. y', z +. z')

let next point dt_val =
  let x, y, z = point in
  let point' = (!s *. (y -. x), x *. (!r -. z) -. y, x *. y -. !b *. z) *.* dt_val in
    point +.+ point'


let ssize = string_of_int !size

let draw_point point =
  let x, y, z = point in
  let x' = !size / 2 + int_of_float (!scale *. (x -. z /. 2.0)) in
  let y' = !size / 2 + int_of_float (!scale *. (y -. z /. 2.0)) in
    Graphics.plot x' y'

let rec do_it point dt_val = function
  | 0 -> ()
  | n -> let () = draw_point point in
      do_it (next point dt_val) dt_val (n - 1)

let rec wait_close () =
  try
    let event = Graphics.wait_next_event [Graphics.Key_pressed] in
    if event.Graphics.key <> '\027' then wait_close ()
  with _ -> ()

let save_picture () =
  match !picture_file with
  | None -> ()
  | Some filename ->
    let w = Graphics.size_x () in
    let h = Graphics.size_y () in
    let img = Graphics.get_image 0 0 w h in
    let pixels = Graphics.dump_image img in
    let oc = open_out filename in
    let () = Printf.fprintf oc "P3\n%d %d\n255\n" w h in
    let () = Array.iter (fun row ->
      Array.iter (fun color ->
        let r = (color lsr 16) land 0xFF in
        let g = (color lsr 8) land 0xFF in
        let b = color land 0xFF in
        Printf.fprintf oc "%d %d %d " r g b
      ) row;
      output_char oc '\n'
    ) pixels in
    close_out oc

;;
let () = Graphics.open_graph (" " ^ ssize ^ "x" ^ ssize) in
let () = do_it (0.01, 0.01, 0.01) !dt !generations in
let () = wait_close () in
save_picture ()
