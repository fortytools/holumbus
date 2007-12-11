# Making all examples

EXAMPLES	= simplesearch

all :
	$(foreach i,$(EXAMPLES),$(MAKE) -C $i all ;)

clean :
	$(foreach i,$(EXAMPLES),$(MAKE) -C $i $@ ;)
