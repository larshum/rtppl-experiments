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
	rm -f r1 r2 r1-inp r1-outp r2-inp r2-outp reader writer relay rtppl network.json
