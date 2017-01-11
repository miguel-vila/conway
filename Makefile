SOURCES = $(wildcard src/*.elm)
COMPILED = elm-conway.js
ELM_MAKE = elm-make

default: $(COMPILED)

$(COMPILED): $(SOURCES)
	$(ELM_MAKE) src/Conway.elm --output=$@

clean:
	rm $(COMPILED)
