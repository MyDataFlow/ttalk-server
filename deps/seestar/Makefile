.PHONY: all compile clean deps doc check_plt build_plt clean_plt dialyze xref test

all: deps compile doc

deps:
	@./rebar get-deps

compile:
	@./rebar compile

clean:
	@./rebar clean

doc:
	@./rebar skip_deps=true doc

APPS = asn1 compiler crypto edoc erts gs hipe inets kernel mnesia \
	public_key runtime_tools ssl stdlib syntax_tools xmerl
PLT = .seestar_plt

check_plt: compile
	dialyzer --check_plt --plt $(PLT) --apps $(APPS) deps/*/ebin

build_plt: compile
	dialyzer --build_plt --output_plt $(PLT) --apps $(APPS) deps/*/ebin

clean_plt:
	rm $(PLT)

dialyze: compile
	dialyzer -Wno_return --plt $(PLT) ebin

xref:
	@./rebar skip_deps=true xref

test:
	@./rebar skip_deps=true eunit
