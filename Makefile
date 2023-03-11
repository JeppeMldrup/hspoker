tests:
	ghc -o test -main-is Test.Hand_logic.main Test/testHandLogic.hs
	./test

clean:
	@rm Test/*.o Test/*.hi Src/*.o Src/*.hi ./test

test: tests clean