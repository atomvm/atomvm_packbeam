##
## Copyright (c) dushin.net
## All rights reserved.
##

all: compile escript edoc etest

compile:
	rebar3 compile

escript:
	rebar3 escriptize

edoc:
	rebar3 edoc

etest:
	rebar3 eunit --cover
	rebar3 proper --cover
	rebar3 cover --verbose

clean:
	rm -rf _build

publish:
	rebar3 hex publish
