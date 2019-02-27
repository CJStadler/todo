MAIN := src/Main.elm
SRC_FILES := $(shell find src/ -name '*.elm')
TEST_FILES := $(shell find tests/ -name '*.elm')
SITE_DIR := site
BUILD_JS := $(SITE_DIR)/elm.js

$(BUILD_JS): $(SRC_FILES)
	elm make $(MAIN) --output=$@

.PHONY: test clean optimize

optimize: $(SRC_FILES)
	elm make src/Main.elm --optimize --output=$(BUILD_JS) \
		| uglifyjs $(BUILD_JS) --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
		| uglifyjs --mangle --output=$(BUILD_JS)

test: $(SRC_FILES)
	npx elm-test

clean:
	rm $(BUILD_JS)

