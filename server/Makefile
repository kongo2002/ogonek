SRCS     := $(wildcard src/*)
INCLUDES := $(wildcard include/*)

.PHONY: all clean start test release build

all: build

build: $(SRCS) $(INCLUDES)
	rebar3 compile

release: all
	rebar3 release

start: all
	@./start.sh

test:
	rebar3 eunit

clean:
	@rm -rf _build

rebuild: clean all
