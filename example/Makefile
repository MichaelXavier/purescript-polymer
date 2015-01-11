DEPS=$(shell find bower_components/*/src -name '*.purs' -type f)
SRCS=$(shell find src -name '*.purs' -type f)

all : $(DEPS) $(SRCS)
	psc-make $^
