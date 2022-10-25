type time_stamped_value = int * Obj.t

external lv_read_stub : int -> time_stamped_value = "lv_read_stub"
external lv_write_stub : int -> time_stamped_value -> unit = "lv_write_stub"

let lv_read (id : int) : time_stamped_value =
  let (ts, value) = lv_read_stub id in
  (ts, Marshal.from_bytes (Obj.obj value) 0)

let lv_write (id : int) (tsv : time_stamped_value) : unit =
  let (ts, value) = tsv in
  lv_write_stub id (ts, Obj.repr (Marshal.to_bytes value []))

let read_binary (ch : in_channel) : Obj.t =
  Obj.repr (input_value ch)

let read_float (ch : in_channel) : float =
  let b = Bytes.create 8 in
  let n = input ch b 0 8 in
  assert (n == 8);
  let v = Bytes.get_int64_ne b 0 in
  Int64.float_of_bits v

let write_binary (ch : out_channel) (data : Obj.t) : unit =
  output_value ch (Obj.obj data)

let write_float (ch : out_channel) (data : float) : unit =
  let b = Bytes.create 8 in
  let v = Int64.bits_of_float data in
  Bytes.set_int64_ne b 0 v;
  output ch b 0 8

type signal = int

let set_signal_handler (x : signal) (f : signal -> unit) : unit =
  Sys.set_signal x (Sys.Signal_handle f)

external clock_get_time : unit -> int * int = "clock_get_time_stub"
external clock_nanosleep : int * int -> unit = "clock_nanosleep_stub"
