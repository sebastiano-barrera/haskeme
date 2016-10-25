
SRC = $(wildcard *.hs)
haskeme: $(SRC)
	ghc --make Main.hs -odir objs -hidir hi -o $@
