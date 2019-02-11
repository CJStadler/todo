MAIN := src/Main.elm
SRC_FILES := $(wildcard src/*.elm)

elm.js: $(SRC_FILES)
	elm make $(MAIN) --output=$@

clean:
	rm elm.js

