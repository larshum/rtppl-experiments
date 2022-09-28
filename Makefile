default: producer recorder
	./producer& sleep 0.1 && ./recorder

producer: producer.c
	$(CC) $^ rtppl/smemio.c -o $@

recorder: recorder.mc
	mi compile $^

clean:
	rm trace-fs*.txt producer recorder
