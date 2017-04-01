# Makefile for FLP project1 - SIMPLIFY-BKG
# Author : Studena Zuzana, xstude22
# Date : 1.4.2017   
FILENAME = simplify-bkg
ARCHNAME = flp-fun-xstude22

all:
	ghc --make $(FILENAME).hs

runi: all
	./$(FILENAME) -i test.in

run1: all
	./$(FILENAME) -1 test.in

run2: all
	./$(FILENAME) -2 test.in

clean:
	rm *.hi *.o $(FILENAME)	
	rm Types/*.hi Types/*.o

cleanZip:
	rm $(ARCHNAME).zip

zip: 
	zip $(ARCHNAME).zip *.hs Types/*.hs Makefile README.md test.in

	