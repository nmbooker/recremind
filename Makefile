all:
	ghc -Wall -threaded recremind.hs -o recremind

clean:
	$(RM) *.hi *.o recremind Recremind/*.hi Recremind/*.o
