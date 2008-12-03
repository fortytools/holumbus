# Making all examples

MASTER            = MapReduceMaster
ALLPROGRAMS       = $(MASTER)


all :
	$(foreach i,$(ALLPROGRAMS),$(MAKE) -C $i all ;)

master :
	$(foreach i,$(MASTER),$(MAKE) -C $i all ;)

wc :
	@wc -l `find . -wholename './_darcs/*' -prune -o -name "*.hs" -print`

clean :
	$(foreach i,$(ALLPROGRAMS),$(MAKE) -C $i $@ ;)
