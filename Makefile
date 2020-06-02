top:

.PHONY: clean
clean:
	rm -f $(shell find Haskell -regex ".*\.\(hi\|o\)$$")
	rm -rf Haskell/BlarneyPlugins/Namer/dist
	make -C Examples clean
