ELVIS_IN_PATH := $(shell elvis --version 2> /dev/null)
ELVIS_LOCAL := $(shell .elvis/_build/default/bin/elvis --version 2> /dev/null)

elvis_rock:
ifdef ELVIS_IN_PATH
	elvis rock
else ifdef ELVIS_LOCAL
	.elvis/_build/default/bin/elvis rock
else
	$(MAKE) compile_elvis
	.elvis/_build/default/bin/elvis rock
endif

compile_elvis:
	git clone https://github.com/inaka/elvis.git --branch 6.0.0 --single-branch .elvis && \
	cd .elvis && \
	rebar3 compile && \
	rebar3 escriptize && \
	cd ..

.PHONY: elvis_rock compile_elvis
