.PHONY: deps clean compile

compile: deps
	rebar compile

deps:
	rebar get-deps

clean:
	rebar clean

eunit: compile
	rebar eunit skip_deps=true
