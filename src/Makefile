all:
	alex -g Generated/LexGrammar.x
	happy -gca Generated/ParGrammar.y
	ghc --make TestGrammar.hs -o ../bin/test

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi ./Generated/*.log ./Generated/*.hi ./Generated/*.o
	
	
	

