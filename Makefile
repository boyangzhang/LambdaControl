make:
	@ghc -o lc Main.hs
	@rm *.hi *.o

clean:
	@rm lc
