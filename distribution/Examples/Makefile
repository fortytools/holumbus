# Making all examples

ALLEXAMPLES       = ClientServer Distribution Ports


all :
	$(foreach i,$(ALLEXAMPLES),$(MAKE) -C $i all ;)

wc :
	@wc -l `find . -wholename './_darcs/*' -prune -o -name "*.hs" -print`

clean :
	$(foreach i,$(ALLEXAMPLES),$(MAKE) -C $i $@ ;)
