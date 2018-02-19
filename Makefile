top:
	

.PHONY: clean
clean:
	rm -f Haskell/*.hi Haskell/*.o
	rm -f Haskell/Blarney/*.hi Haskell/Blarney/*.o
	make -C Examples clean
