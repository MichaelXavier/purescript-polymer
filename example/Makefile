DEPS=$(shell find bower_components/*/src -name '*.purs' -type f)
SRCS=$(shell find src -name '*.purs' -type f)

all : node_modules/Build/index.js components/counter.html

node_modules/Build/index.js: $(DEPS) $(SRCS)
	psc-make $^ --output node_modules

components/counter.html: $(DEPS) $(SRCS)
	node --harmony-proxies -e 'require("Build").main();'
