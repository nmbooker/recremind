all: recremind

recremind: recremind.hs Recremind/Templates.hs
	ghc -Wall -threaded recremind.hs -o recremind

clean:
	$(RM) *.hi *.o recremind Recremind/*.hi Recremind/*.o
