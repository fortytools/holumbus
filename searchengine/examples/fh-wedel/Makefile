# Making FH Wedel Search

GHC_FLAGS	= -Wall -O2 
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

SOURCE    = source
OUTPUT    = output

SHADER    = $(SOURCE)/FHW/Search.hs

PROGS     = Indexer

all : shader $(PROGS)

shader : $(SHADER) output
	$(GHC) -threaded --make $< -hidir $(OUTPUT) -odir $(OUTPUT) -i./$(SOURCE)

indexer : Indexer
hebalizer : Hebalizer

% : $(SOURCE)/FHW/%.hs output
	$(GHC) -threaded --make -o $@ $< -hidir $(OUTPUT) -odir $(OUTPUT) -i./$(SOURCE)

output :
	mkdir -p $(OUTPUT)

install :
	@[ $(JANUS_ROOT) ] || ( echo "JANUS_ROOT environment variable not set" 1>&2 ; exit 1 )
	@[ -d $(JANUS_ROOT) ] || ( echo "Janus root dir not found" 1>&2 ; exit 1 )
	@[ -w $(JANUS_ROOT) ] || ( echo "Write access to Janus root dir required" 1>&2 ; exit 1 )
	cp -r wwwpages/* $(JANUS_ROOT)/wwwpages/
	[ -d $(JANUS_ROOT)/indexes ] || mkdir $(JANUS_ROOT)/indexes
	cp -r index/* $(JANUS_ROOT)/indexes/
	[ -d $(JANUS_ROOT)/conf ] || mkdir $(JANUS_ROOT)/conf
	cp conf/server.xml $(JANUS_ROOT)/conf/
	[ -d $(JANUS_ROOT)/FHW ] || mkdir $(JANUS_ROOT)/FHW
	cp output/FHW/Search.* $(JANUS_ROOT)/FHW/
#	cp output/FHW/Parser.* $(JANUS_ROOT)/FHW/
#	cp output/FHW/Common.* $(JANUS_ROOT)/FHW/

clean :
	$(RM) $(OUTPUT) $(PROGS)
