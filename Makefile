TRACE_INPUTS=trace-0 trace-1 trace-2 trace-3 trace-4 trace-5 trace-6 trace-7 trace-8
TRACE_OUTPUTS=trace-9 trace-10 trace-11
MAP_FILE=maps/map-with-wall

$(TRACE_OUTPUTS): out $(TRACE_INPUTS)
	./out --room-map $(MAP_FILE).txt --replay

out: position-model.mc argparse.mc buffers.mc room.mc
	cppl $<

$(TRACE_INPUTS): producer
	./$< $(MAP_FILE).txt

producer: producer.mc
	mi compile $< --output $@

clean:
	rm -rf $(TRACE_INPUTS) $(TRACE_OUTPUTS) out out.mc producer
