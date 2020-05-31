top:

.PHONY: clean
clean:
	rm -f `find . -regex ".*\.\(hi\|o\)"`
	rm -rf Haskell/BlarneyPlugins/Namer/dist
	make -C Examples clean
