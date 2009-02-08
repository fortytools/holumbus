# Making all examples

FILESYSTEM        = FileSystem
UTILS             = Utils/Filehandling
ALLEXAMPLES       = $(FILESYSTEM) $(UTILS)


all :
	$(foreach i,$(ALLEXAMPLES),$(MAKE) -C $i all ;)

wc :
	@wc -l `find . -wholename './_darcs/*' -prune -o -name "*.hs" -print`

clean :
	$(foreach i,$(ALLEXAMPLES),$(MAKE) -C $i $@ ;)
