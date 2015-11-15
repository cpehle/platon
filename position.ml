open Core.Std
type t = { filename : string  Option.t;
           line : int;
           lineoffset : int;
           columnoffset : int
         }
let default_position = None, 1, 0, 0
let to_string p = let fn = match p.filename with
                    | None -> "<none>"
                    | Some f -> "<" ^ f ^ ">" in
                 sprintf "{ filename = %s;\
                            line = %i;\
                            lineoffset = %i;\
                            columnoffset = %i\
                           }" fn p.line p.lineoffset p.columnoffset
