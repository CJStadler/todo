MAIN = src/Main.elm

elm.js: $(MAIN)
	elm make $(MAIN) --output=$@

clean:
	rm elm.js

