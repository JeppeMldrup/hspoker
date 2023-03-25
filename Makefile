tests:
	ghc -o test -main-is Test.Main.main Test/testHandLogic.hs Test/testPokerLogic.hs Test/Main.hs
	./test

clean:
	@rm Test/*.o Test/*.hi Src/*.o Src/*.hi ./test

test: tests clean