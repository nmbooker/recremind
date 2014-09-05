all:
	ghc -Wall -fwarn-unused-do-bind -threaded recremind.hs -o recremind

clean:
	$(RM) *.hi *.o recremind Recremind/*.hi Recremind/*.o Atd/*.hi Atd/*.o
