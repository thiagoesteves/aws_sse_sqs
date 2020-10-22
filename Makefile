.PHONY: all test clean compile run check
	
all:
	ulimit -n 4096; ERL_FLAGS=" -args_file ${PWD}/config/vm.args.src" rebar3 shell --apps message
	
test:
	rebar3 ct --cover && rebar3 cover --verbose
	
clean:
	rebar3 clean
	
compile:
	rebar3 compile
	
check:
	rebar3 as test dialyzer
