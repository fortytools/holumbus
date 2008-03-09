# Making Hayoo

HAYOO	= indexer search

all :
	$(foreach i,$(HAYOO),$(MAKE) -C $i all ;)

clean :
	$(foreach i,$(HAYOO),$(MAKE) -C $i $@ ;)