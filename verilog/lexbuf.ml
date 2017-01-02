type t = {
  stream : Sedlexing.lexbuf ;
  mutable pos_start : Position.t ;
  mutable pos_end : Position.t;
  }


let create_lexbuf ?(fn=None) stream =
  let pos_end =
    Position.{
      filename = fn;
      line = 1;
      lineoffset = 0;
      columnoffset = 0;
    }
  in {pos_start = pos_end; pos_end; stream;}

let from_string ?(fn=None) s = create_lexbuf ~fn (Sedlexing.Utf8.from_string s)
let from_channel ?(fn=None) c = create_lexbuf ~fn (Sedlexing.Utf8.from_channel c)


let lexeme { stream } = Sedlexing.Utf8.lexeme stream

let new_line lexbuf =
  let open Sedlexing in
  let lcp = lexbuf.pos_end in
  lexbuf.pos_end <- Position.{ lcp with line = lcp.line+1; lineoffset = lcp.columnoffset}

let update_position ({pos_end; pos_start; stream} as buf) =
  let p_start, p_end = Sedlexing.loc stream in
  buf.pos_start <- {pos_start with  Position.columnoffset = p_start};
  buf.pos_end <- {pos_end with Position.columnoffset = p_end }
