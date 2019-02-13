MAIN := src/Main.elm
SRC_FILES := $(wildcard src/*.elm)
TEST_FILES := $(wildcard tests/*.elm)

elm.js: $(SRC_FILES)
	elm make $(MAIN) --output=$@

test: $(SRC_FILES)
	npx elm-test

clean:
	rm elm.js

