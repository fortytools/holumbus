# Making all examples

EXAMPLES	= filesystem sMapReduce dMapReduce portregistry

all :	clean
	$(foreach i,$(EXAMPLES),$(MAKE) -C $i all ;)

wc :
	@wc -l `find . -wholename './_darcs/*' -prune -o -name "*.hs" -print`

clean :
	$(foreach i,$(EXAMPLES),$(MAKE) -C $i $@ ;)
