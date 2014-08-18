all: recremind

recremind: recremind.hs Recremind/Templates.hs
	ghc -W -threaded recremind.hs -o recremind

clean:
	$(RM) *.hi *.o recremind Recremind/*.hi Recremind/*.o
