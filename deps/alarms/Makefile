.PHONY: all compile test clean

all: compile

compile: rebar
	./rebar get-deps compile

test: rebar compile
	./rebar skip_deps=true eunit

ct: rebar compile
	./rebar skip_deps=true ct -v

clean: rebar
	./rebar clean

rebar:
	wget http://cloud.github.com/downloads/basho/rebar/rebar
	chmod u+x rebar
