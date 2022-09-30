type port = int
type time_stamped_value = int * float

external lv_read : port -> time_stamped_value = "lv_read_stub"
external lv_write : port -> time_stamped_value -> unit = "lv_write_stub"

type signal = int

let set_signal_handler (x : signal) (f : signal -> unit) : unit =
  Sys.set_signal x (Sys.Signal_handle f)
