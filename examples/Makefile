# Making all examples

EXAMPLES	= simplesearch benchmark utilities

all :
	$(foreach i,$(EXAMPLES),$(MAKE) -C $i all ;)

clean :
	$(foreach i,$(EXAMPLES),$(MAKE) -C $i $@ ;)
