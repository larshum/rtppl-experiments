TRACE_INPUTS=trace-0 trace-1 trace-2 trace-3 trace-6 trace-7
TRACE_OUTPUTS=trace-8 trace-9

$(TRACE_OUTPUTS): out $(TRACE_INPUTS)
	./out --replay

out: runner.mc distance-model.mc argparse.mc buffers.mc
	cppl $<

$(TRACE_INPUTS): extra/producer
	./$<

extra/producer: extra/producer.mc
	mi compile $< --output $@

clean:
	rm -rf $(TRACE_INPUTS) $(TRACE_OUTPUTS) out out.mc extra/producer
