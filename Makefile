DEPS=$(shell find bower_components/*/src -name '*.purs' -type f)
SRCS=$(shell find src -name '*.purs' -type f)


all: dist/purescript-polymer.min.js .psci deps

deps: bower.json
	bower install

clean :
	rm -f dist/**/*.js

dist/purescript-polymer.js : $(DEPS) $(SRCS)
	psc --module Polymer --module Polymer.Compiler --output $@ $^


%.min.js : %.js
	closure-compiler --compilation_level SIMPLE_OPTIMIZATIONS < $^ > $@

.psci: $(DEPS) $(SRCS)
	rm -f .psci
	for f in $^; do \
		echo :m $$f >> $@; \
	done

watch :
	@while true; do \
		inotifywait -q -r -e modify,close_write,move,create,delete src bower_components; \
		$(MAKE); \
	done

.PHONY: clean watch
