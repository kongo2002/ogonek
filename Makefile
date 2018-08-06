.PHONY: all server client clean rebuild

all: server client

client:
	$(MAKE) -C client

server:
	$(MAKE) -C server

clean:
	$(MAKE) -C client clean
	$(MAKE) -C server clean

rebuild: clean all
