# Making Hayoo!

GHC_FLAGS	= -Wall -O2 
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

SOURCE    = source
OUTPUT    = output

SHADER    = $(SOURCE)/Hayoo/Search.hs

PROGS     = Indexer Hebalizer

all : shader $(PROGS)

shader : $(SHADER) output
	$(GHC) -threaded --make $< -hidir $(OUTPUT) -odir $(OUTPUT) -i./$(SOURCE)

indexer : Indexer
hebalizer : Hebalizer

% : $(SOURCE)/Hayoo/%.hs output
	$(GHC) -threaded --make -o $@ $< -hidir $(OUTPUT) -odir $(OUTPUT) -i./$(SOURCE)

output :
	mkdir -p $(OUTPUT)

install :
	@[ $(JANUS_ROOT) ] || ( echo "JANUS_ROOT environment variable not set" 1>&2 ; exit 1 )
	@[ -d $(JANUS_ROOT) ] || ( echo "Janus root dir not found" 1>&2 ; exit 1 )
	@[ -w $(JANUS_ROOT) ] || ( echo "Write access to Janus root dir required" 1>&2 ; exit 1 )
	cp -r wwwpages/* $(JANUS_ROOT)/wwwpages/
	[ -d $(JANUS_ROOT)/conf ] || mkdir $(JANUS_ROOT)/conf
	cp conf/server.xml $(JANUS_ROOT)/conf/
	[ -d $(JANUS_ROOT)/Hayoo ] || mkdir $(JANUS_ROOT)/Hayoo
	cp output/Hayoo/Search.* $(JANUS_ROOT)/Hayoo/
	cp output/Hayoo/Parser.* $(JANUS_ROOT)/Hayoo/
	cp output/Hayoo/Common.* $(JANUS_ROOT)/Hayoo/

clean :
	$(RM) $(OUTPUT) $(PROGS)
