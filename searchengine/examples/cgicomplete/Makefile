# Making CGI Complete example

GHC_FLAGS	= -Wall -O2 
GHC				= ghc $(GHC_FLAGS)

RM_FLAGS	= -rf
RM				= rm $(RM_FLAGS)

SOURCE    = source
OUTPUT    = output

PROGS     = Server Client

all : $(PROGS)

server : Server
client : Client

% : $(SOURCE)/%.hs output
	$(GHC) -threaded --make -o $@ $< -hidir $(OUTPUT) -odir $(OUTPUT) -i./$(SOURCE)

output :
	mkdir -p $(OUTPUT)

clean :
	$(RM) $(OUTPUT) $(PROGS)
