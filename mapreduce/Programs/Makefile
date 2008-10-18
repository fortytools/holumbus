# Making all examples

# COMMUNICATION     = Communication/ClientServer Communication/Ports
MASTER            = MapReduceMaster
PORTREGISTRY      = PortRegistry
ALLPROGRAMS       = $(MASTER) $(PORTREGISTRY)


all :
	$(foreach i,$(ALLPROGRAMS),$(MAKE) -C $i all ;)

master :
	$(foreach i,$(MASTER),$(MAKE) -C $i all ;)

registry :
	$(foreach i,$(PORTREGISTRY),$(MAKE) -C $i all ;)

wc :
	@wc -l `find . -wholename './_darcs/*' -prune -o -name "*.hs" -print`

clean :
	$(foreach i,$(ALLPROGRAMS),$(MAKE) -C $i $@ ;)
