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

(* usual values *)
let s = 10.0
let b = 8.0 /. 3.0

(* This one can be varied *)
let r = 28.0

let ( *.* ) point dt =
  let x, y, z = point in
    (x *. dt, y *. dt, z *. dt)

let ( +.+ ) point point' =
  let x, y, z = point in
  let x', y', z' = point' in
    (x +. x', y +. y', z +. z')

let next point dt =
  let x, y, z = point in
  let point' = (s *. (y -. x), x *. (r -. z) -. y, x *. y -. b *. z) *.* dt in
    point +.+ point'


let size = 500
let ssize = string_of_int size
let scale = 5.0

let generations = 100000

let rec draw_point point =
  let x, y, z = point in
  let x' = size / 2 + int_of_float (scale *. (x -. z /. 2.0)) in
  let y' = size / 2 + int_of_float (scale *. (y -. z /. 2.0)) in
    Graphics.plot x' y'

let rec do_it point dt = function
  | 0 -> ()
  | n -> let () = draw_point point in
      do_it (next point dt) dt (n - 1)

;;
let () = Graphics.open_graph (" " ^ ssize ^ "x" ^ ssize) in
let () = do_it (0.01, 0.01, 0.01) 0.001 generations in
read_line ()
