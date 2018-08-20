.PHONY: multiplication run

run: multiplication
	./multiplication

multiplication: multiplication.hs
	ghc -o $@ $<


