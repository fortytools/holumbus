# Making all examples

EXAMPLES	= filesystem distribution

all :
	$(foreach i,$(EXAMPLES),$(MAKE) -C $i all ;)

wc :
	@wc -l `find . -wholename './_darcs/*' -prune -o -name "*.hs" -print`

clean :
	$(foreach i,$(EXAMPLES),$(MAKE) -C $i $@ ;)
