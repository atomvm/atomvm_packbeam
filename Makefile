##
## Copyright (c) dushin.net
## All rights reserved.
##

all: compile escript edoc etest release

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

release:
	rebar3 release
	rebar3 tar

clean:
	rm -rf _build

publish:
	rebar3 hex publish
