# Making all examples

ALLEXAMPLES       = ChatDChan ChatDFunction CounterDFunction CounterDMVar HelloWorld

all :
	$(foreach i,$(ALLEXAMPLES),$(MAKE) -C $i all ;)

wc :
	@wc -l `find . -wholename './_darcs/*' -prune -o -name "*.hs" -print`

clean :
	$(foreach i,$(ALLEXAMPLES),$(MAKE) -C $i $@ ;)
