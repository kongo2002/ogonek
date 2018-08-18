.PHONY: all server client clean rebuild tags

all: server client

client:
	$(MAKE) -C client

server:
	$(MAKE) -C server

clean:
	@rm -f tags
	$(MAKE) -C client clean
	$(MAKE) -C server clean

tags:
	@ctags -R server/src server/include client/src

rebuild: clean all
