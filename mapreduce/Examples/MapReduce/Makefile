# Making all examples

ALLEXAMPLES       = Crawler WordFrequency
STANDALONE        = Standalone


all :
	$(foreach i,$(ALLEXAMPLES),$(MAKE) -C $i all ;)

standalone :	
	$(foreach i,$(STANDALONE),$(MAKE) -C $i all ;)
	
wc :
	@wc -l `find . -wholename './_darcs/*' -prune -o -name "*.hs" -print`

clean :
	$(foreach i,$(ALLEXAMPLES),$(MAKE) -C $i $@ ;)
	$(foreach i,$(STANDALONE),$(MAKE) -C $i $@ ;)
