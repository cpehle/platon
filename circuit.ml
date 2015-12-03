open Core.Std

let rec for_each items f =
   match items with
    | [] -> ()
    | x::xs -> (f x; for_each xs f)

let rec call_each = function
 | [] -> ()
 | p::ps -> ( p(); call_each ps )

type signal = Hi | Lo

type wire = { get_signal_rec : unit -> signal;
              set_signal_rec : signal -> unit;
              add_action_rec : (unit->unit)->unit; }

let get_signal pwire = pwire.get_signal_rec()
let set_signal pwire new_value = pwire.set_signal_rec new_value
let add_action pwire action_procedure =  pwire.add_action_rec action_procedure

let make_wire () =
   let signal_value = ref Lo
   and action_procedures = ref [] in
   let set_my_signal new_value =
      if !signal_value <> new_value
         then
            begin
               signal_value := new_value;
               call_each (!action_procedures)
            end
         else ()
   and accept_action_procedure proc =
      action_procedures := proc :: !action_procedures
   and get_signal () = !signal_value
   in
      { get_signal_rec = get_signal;
        set_signal_rec = set_my_signal;
        add_action_rec = accept_action_procedure; }

let logical_not = function
 | Lo -> Hi
 | Hi -> Lo

let logical_and s1 s2 =
   match s1, s2 with
      | Hi, Hi -> Hi
      | _ -> Lo

let logical_or  s1 s2 =
   match s1, s2 with
      | Lo, Lo -> Lo
      | _ -> Hi

module ProcQueue = Queue(struct type t=(unit->unit) end)

type timesegment = TimeSegment of int ref * ProcQueue.typ
let make_time_segment time queue = TimeSegment(ref time, queue)
let segment_time (TimeSegment(time, q)) = time
let segment_queue (TimeSegment(time, q)) = q

(* agenda is a list of time segments *)
exception Agenda of string
let make_agenda () = MList.cons (make_time_segment 0 ProcQueue.empty) MList.MNil
let current_time agenda = !(segment_time(MList.car agenda))
let current_time_ref agenda = segment_time(MList.car agenda)
let set_current_time agenda time = (current_time_ref agenda) := time

let segments agenda = MList.cdr agenda
let set_segments agenda segs = MList.set_cdr agenda segs
let first_segment agenda = MList.car(segments agenda)
let rest_segments agenda = MList.cdr(segments agenda)

let empty_agenda agenda = (segments agenda = MList.MNil)

let first_agenda_item agenda =
   if empty_agenda agenda
      then raise (Agenda "Agenda is empty -- FIRST-AGENDA-ITEM")
      else
         let first_seg = first_segment agenda
         in
            begin
               set_current_time agenda !(segment_time first_seg);
               ProcQueue.front(segment_queue first_seg)
            end

let remove_first_agenda_item agenda =
   let q = segment_queue(first_segment agenda)
   in
      begin
         ProcQueue.delete q;
         if ProcQueue.is_empty q
            then set_segments agenda (rest_segments agenda)
            else ()
      end

let add_to_agenda time action agenda =
   let belongs_before = function
    | MList.MNil -> true
    | segments -> (time < !(segment_time(MList.car segments))) in
   let make_new_time_segment time action =
      let q = ProcQueue.empty
      in
         begin
            ProcQueue.insert q action;
            make_time_segment time q
         end in
   let rec add_to_segments segments =
      if !(segment_time(MList.car segments)) = time
         then ProcQueue.insert (segment_queue(MList.car segments)) action
         else
            let rest = MList.cdr segments
            in
               if belongs_before rest
                  then MList.set_cdr segments (MList.cons (make_new_time_segment time action) (MList.cdr segments))
                  else add_to_segments rest
   and segs = segments agenda
   in
      if belongs_before segs
         then set_segments agenda (MList.cons (make_new_time_segment time action) segs)
         else add_to_segments segs

let the_agenda = make_agenda()
let after_delay delay action =
   add_to_agenda (delay + (current_time the_agenda)) action the_agenda

let inverter_delay = 2
let and_gate_delay = 3
let or_gate_delay = 5

let inverter input output =
   let new_value = logical_not(get_signal input) in
   let invert_input () =
      after_delay inverter_delay  (fun () -> set_signal output new_value)
   in add_action input invert_input

let and_gate a1 a2 output =
   let new_value = logical_and (get_signal a1) (get_signal a2) in
   let and_action_procedure () =
         after_delay and_gate_delay (fun () -> set_signal output new_value)
   in
      begin
         add_action a1 and_action_procedure;
         add_action a2 and_action_procedure
      end

let or_gate a1 a2 output =
   let new_value = logical_or (get_signal a1) (get_signal a2) in
   let or_action_procedure () =
      after_delay or_gate_delay (fun () -> set_signal output new_value)
   in
      begin
         add_action a1 or_action_procedure;
         add_action a2 or_action_procedure
      end

let half_adder a b s c =
   let d = make_wire()
   and e = make_wire()
   in
      begin
         or_gate a b d;
         and_gate a b c;
         inverter c e;
         and_gate d e s
      end

let or_gate a1 a2 output =
   let b = make_wire()
   and c = make_wire()
   and d = make_wire()
   in
      begin
         inverter a1 b;
         inverter a2 c;
         and_gate b c d;
         inverter d output
      end

let a = make_wire()
let b = make_wire()
let c = make_wire()
let d = make_wire()
let e = make_wire()
let s = make_wire();;

or_gate a b d;;
and_gate a b c;;
inverter c e;;
and_gate d e s;;

let full_adder a b c_in sum c_out =
   let s = make_wire()
   and c1 = make_wire()
   and c2 = make_wire()
   in
      begin
         half_adder b c_in s c1;
         half_adder a s sum c2;
         or_gate c1 c2 c_out
      end

let rec propagate () =
   if empty_agenda the_agenda
      then ()
      else
         let first_item = first_agenda_item the_agenda
         in
            begin
               first_item();
               remove_first_agenda_item the_agenda;
               propagate()
            end

let signal_to_string = function
 | Hi -> "Hi"
 | Lo -> "Lo"

let probe name pwire =
   add_action
      pwire
      (fun () ->
         begin
            print_string name;
            print_string " ";
            print_string (string_of_int (current_time the_agenda));
            print_string "  New-value = ";
            print_string (signal_to_string(get_signal pwire));
            print_string "\n"
         end)

(* Sample simulation *)
let input_1 = make_wire()
let input_2 = make_wire()
let sum = make_wire()
let carry = make_wire();;

probe "sum" sum;;
probe "carry" carry;;

half_adder input_1 input_2 sum, carry;;
set_signal input_1 Hi;;
propagate();;

set_signal input_2 Hi;;
propagate();;
