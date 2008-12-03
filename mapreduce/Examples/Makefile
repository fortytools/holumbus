# Making all examples

FILESYSTEM        = FileSystem
MAPREDUCE         = MapReduce
UTILS             = Utils/Filehandling Utils/PickleTest
ALLEXAMPLES       = $(FILESYSTEM) $(MAPREDUCE) $(UTILS)


all :
	$(foreach i,$(ALLEXAMPLES),$(MAKE) -C $i all ;)

mr :
	$(foreach i,$(MAPREDUCE),$(MAKE) -C $i all ;)
	
wc :
	@wc -l `find . -wholename './_darcs/*' -prune -o -name "*.hs" -print`

clean :
	$(foreach i,$(ALLEXAMPLES),$(MAKE) -C $i $@ ;)
