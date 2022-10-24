TRACE_INPUTS=trace-0.txt trace-1.txt trace-6.txt trace-7.txt

trace-8.txt: out $(TRACE_INPUTS)
	./out --replay

out: runner.mc distance-model.mc argparse.mc buffers.mc
	cppl $<

$(TRACE_INPUTS): extra/producer
	./$<

extra/producer: extra/producer.mc
	mi compile $< --output $@

clean:
	rm -rf trace-*.txt out out.mc extra/producer
