open Core.Std
type filename = string
type t = (filename Option.t * int * int)
let default_position = None, 0, 0
