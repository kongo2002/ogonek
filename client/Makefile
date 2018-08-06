YARN    := $(shell command -v yarn 2> /dev/null)
SRCS    := $(wildcard src/*)
TARGETS := dist/index.html dist/static/app.css dist/static/app.js

.PHONY: all build clean rebuild

all: build

build: $(TARGETS)

$(TARGETS): $(SRCS)
ifndef YARN
	npm install
	npm run build
else
	yarn
	yarn build
endif

clean:
	@rm -f $(TARGETS)

rebuild: clean all
