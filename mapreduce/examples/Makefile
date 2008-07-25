# Making all examples

EXAMPLES	= filesystem distribution standalone ports pickle

all :	clean
	$(foreach i,$(EXAMPLES),$(MAKE) -C $i all ;)

wc :
	@wc -l `find . -wholename './_darcs/*' -prune -o -name "*.hs" -print`

clean :
	$(foreach i,$(EXAMPLES),$(MAKE) -C $i $@ ;)
