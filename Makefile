TRACE_INPUTS=trace-0 trace-1 trace-2 trace-3 trace-4 trace-5 trace-6 trace-7 trace-8
TRACE_OUTPUTS=trace-9 trace-10 trace-11
MAP_FILE=maps/simulation-map

out: position-model.mc argparse.mc buffers.mc room.mc shared.mc
	cppl $<

plot:
	./out --print-pos-dist trace-11 | python3 scripts/pos-plot.py $(MAP_FILE).png

$(TRACE_INPUTS): producer
	./$< $(MAP_FILE).txt

clean:
	rm -rf $(TRACE_INPUTS) $(TRACE_OUTPUTS) out out.mc producer
