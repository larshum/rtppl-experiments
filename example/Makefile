# Replace with your local path to try it out yourself
RTPPL_PATH=~/Documents/Miking/dppl-miking/coreppl/src/rtppl

all: example.rpl rtppl reader writer relay
	./rtppl --out-path . $<

reader: reader.c common.h
	gcc $< -o $@

writer: writer.c common.h
	gcc $< -o $@

relay: relay.c common.h
	gcc $< -o $@

rtppl: $(RTPPL_PATH)/rtppl.mc $(RTPPL_PATH)/ast.mc
	mi compile $< --output $@

$(RTPPL_PATH)/ast.mc: $(RTPPL_PATH)/ast.syn
	mi syn $< $@

clean:
	rm -f r1 r2 r1-in1 r1-out1 r2-in1 r2-out1 reader writer relay rtppl
