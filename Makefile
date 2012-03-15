ERL ?= erl
APP := restclient

.PHONY: deps test

all: deps compile

compile:
	@./rebar compile

debug:
	@./rebar debug_info=1 compile

deps:
	@./rebar get-deps

app:
	@./rebar compile skip_deps=true

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

test:
	@./rebar compile skip_deps=true eunit

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

## Dialyzer support
APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
           xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = .dialyzer_plt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) ebin

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) ebin

dialyzer: compile
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) ebin

