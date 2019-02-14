MAIN := src/Main.elm
SRC_FILES := $(shell find src/ -name '*.elm')
TEST_FILES := $(shell find tests/ -name '*.elm')
SITE_DIR := site

$(SITE_DIR)/elm.js: $(SRC_FILES)
	elm make $(MAIN) --output=$@

.PHONY: test clean

test: $(SRC_FILES)
	npx elm-test

clean:
	rm $(SITE_DIR)/elm.js

