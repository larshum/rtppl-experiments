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
  Marshal.from_channel ch

let write_binary (ch : out_channel) (data : Obj.t) : unit =
  Marshal.to_channel ch (Obj.obj data) []

type signal = int

let set_signal_handler (x : signal) (f : signal -> unit) : unit =
  Sys.set_signal x (Sys.Signal_handle f)

external clock_get_time : unit -> int * int = "clock_get_time_stub"
external clock_nanosleep : int * int -> unit = "clock_nanosleep_stub"
