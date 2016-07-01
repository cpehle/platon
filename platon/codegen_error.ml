type t = Error of string

let to_string = function
  | Error s -> s
