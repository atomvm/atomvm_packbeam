##
## Copyright (c) dushin.net
## All rights reserved.
##

all: compile escript etest doc rel

compile:
	rebar3 compile

escript:
	rebar3 escriptize

doc:
	rebar3 as doc ex_doc

etest:
	rebar3 as test eunit --cover
	rebar3 as test proper --cover
	rebar3 as test cover --verbose

rel:
	rebar3 as prod release
	rebar3 as prod tar
	rm -rf x
	mkdir x
	PACKBEAM_DEBUG=1 ./install.sh x 0.7.1
	x/bin/packbeam version

clean:
	rm -rf _build

publish: doc
	rebar3 as publish hex publish --doc-dir docs
