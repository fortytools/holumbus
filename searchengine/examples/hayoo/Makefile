# Making Hayoo!

GHC_FLAGS	= -Wall -O2 
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

SOURCE		= source
OUTPUT		= output

# PROGS		= Indexer Hebalizer IndexerExperimental WorkerMain ClientMain
PROGS		= Indexer IndexerExperimental WorkerMain ClientMain

all		: $(PROGS) janus

indexer		: Indexer
hebalizer	: Hebalizer
experimental : IndexerExperimental
worker  : WorkerMain
client : ClientMain

# ------------------------------------------------------------
# old stuff for janus with hs-plugins

shader : $(SHADER) output
	$(GHC) -threaded --make $< -hidir $(OUTPUT) -odir $(OUTPUT) -i./$(SOURCE)

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
	@echo '# cp output/Hayoo/Search.* $(JANUS_ROOT)/Hayoo/'
	@echo '# cp output/Hayoo/Parser.* $(JANUS_ROOT)/Hayoo/'
	@echo '# cp output/Hayoo/Common.* $(JANUS_ROOT)/Hayoo/'
	@echo '# cp output/Hayoo/HTML.* $(JANUS_ROOT)/Hayoo/'

# ------------------------------------------------------------

clean :
	$(MAKE) cleanjanus
	$(RM) $(OUTPUT) $(PROGS)

# ------------------------------------------------------------
#
# build a statically linked janus server
#
# all servlet sources must be located under source subdir
# and must be in the package Network.Server.Janus.Shader

SERVLETS	:= $(wildcard $(SOURCE)/Hayoo/*.hs)
CONF		= conf/server.xml

# janus-conf is installed togeher with the janus-library
JANUSCONF	= janus-conf

# 1. step: generate the Janus main from server.xml
Janus.hs	: $(CONF)
		$(JANUSCONF) $(CONF) $@


# 2. step: generate the executable janus from the main prog
janus		: Janus.hs $(SERVLETS)
		$(GHC) -threaded -o $@ --make -i./source -package hslogger -package json Janus.hs

cleanjanus	:
		rm -f Janus.hs Janus.o Janus.hi $(SERVLETS:.hs=.o) $(SERVLETS:.hs=.hi)


# ------------------------------------------------------------
#
# build a statically linked janus server

CONF2		= conf/server2.xml
HAYOOSHADER	= source/Hayoo/Shader.hs
HAYOOSHADER2	= source/Hayoo/Shader2.hs
HAYOOINDEX	= hayoo-index.bin
HAYOOINDEX2	= hayoo-inverted-mem-serialized-bzip.bin
HAYOOINDEXROOT	= ../../test/HayooIndex

# 0. step: generate and modify server2.xml

$(CONF2)	: $(CONF)
		  cat $< | \
		  sed -e 's|$<|$@|g' \
		      -e 's|Hayoo.Shader|Hayoo.Shader2|g' \
		      -e 's|"indexes/$(HAYOOINDEX)"|"indexes/$(HAYOOINDEX2)"|g' \
		  > $@

# 1. step: generate Hayoo.Shader2 source
$(HAYOOSHADER2)	: $(HAYOOSHADER)
		  cat $< | \
		  sed -e 's|Hayoo.Shader|Hayoo.Shader2|g' \
		      -e 's|Persistent|InvertedOSerialized|g' \
		      -e 's|"$(HAYOOINDEX)"|"$(HAYOOINDEX2)"|g' \
		      -e 's|OneFile|CompressedPrefixMem|g' \
		      -e 's|y|y|g' \
		  > $@

# 1. step: generate the Janus main from server.xml
Janus2.hs	: $(CONF2)
		$(JANUSCONF) $(CONF2) $@


# 2. step: generate the executable janus from the main prog
janus2		: Janus2.hs $(SERVLETS) $(HAYOOSHADER2)
		$(GHC) -threaded -o $@ --make -i./source -package hslogger -package json $<

install2 :
		@[ $(JANUS_ROOT) ] || ( echo "JANUS_ROOT environment variable not set" 1>&2 ; exit 1 )
		@[ -d $(JANUS_ROOT) ] || ( echo "Janus root dir not found" 1>&2 ; exit 1 )
		@[ -w $(JANUS_ROOT) ] || ( echo "Write access to Janus root dir required" 1>&2 ; exit 1 )
		cp -r wwwpages/* $(JANUS_ROOT)/wwwpages/
		[ -d $(JANUS_ROOT)/conf ] || mkdir $(JANUS_ROOT)/conf
		cp $(CONF2) $(JANUS_ROOT)/$(CONF2)
		cp janus2 $(JANUS_ROOT)/janus2
		[ -d $(JANUS_ROOT)/Hayoo ] || mkdir $(JANUS_ROOT)/Hayoo
		[ -d $(JANUS_ROOT)/indexes ] || mkdir $(JANUS_ROOT)/indexes
		$(MAKE) $(JANUS_ROOT)/indexes/$(HAYOOINDEX2)
		@echo 'now exec ( cd $(JANUS_ROOT) ; ./janus2 -RTS -M150M +RTS )'

$(JANUS_ROOT)/indexes/$(HAYOOINDEX2)	: $(HAYOOINDEXROOT)/indexes/$(HAYOOINDEX2)
		cp $< $@

cleanjanus2	:
		rm -f Janus2.hs Janus2.o Janus2.hi $(SERVLETS:.hs=.o) $(SERVLETS:.hs=.hi) $(HAYOOSHADER2) $(CONF2)

# ------------------------------------------------------------
