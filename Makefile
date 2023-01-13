TRACE_INPUTS=trace-0 trace-1 trace-2 trace-3 trace-4 trace-5 trace-6 trace-7 trace-8 trace-9
TRACE_OUTPUTS=trace-10 trace-11 trace-12 replay-trace-12
MAP_FILE=maps/default-sim

out: position-model.mc argparse.mc buffers.mc room.mc shared.mc sm_conf.mc init-pos.mc
	cppl $<

plot:
	./out --print-pos-dist trace-12 | python3 scripts/pos-plot.py $(MAP_FILE).png

$(TRACE_INPUTS): producer
	./$< $(MAP_FILE).txt

clean:
	rm -rf $(TRACE_INPUTS) $(TRACE_OUTPUTS) out out.mc producer
