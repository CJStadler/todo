MAIN := src/Main.elm
SRC_FILES := $(wildcard src/*.elm)
TEST_FILES := $(wildcard tests/*.elm)
SITE_DIR := site

$(SITE_DIR)/elm.js: $(SRC_FILES)
	elm make $(MAIN) --output=$@

.PHONY: test clean

test: $(SRC_FILES)
	npx elm-test

clean:
	rm elm.js

