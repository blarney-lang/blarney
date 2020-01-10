top:

.PHONY: clean
clean:
	rm -f Haskell/*.hi Haskell/*.o
	rm -f Haskell/Blarney/*.hi Haskell/Blarney/*.o
	rm -f Haskell/Blarney/Core/*.hi Haskell/Blarney/Core/*.o
	make -C Examples clean
