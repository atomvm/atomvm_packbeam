##
## Copyright (c) 2019 dushin.net
## All rights reserved.
##
## SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

all: compile escript etest doc rel

compile:
	rebar3 compile

escript:
	rebar3 escriptize

doc:
	rebar3 as doc ex_doc

etest:
	rebar3 as test escriptize
	rebar3 as test eunit --cover
	rebar3 as test proper --cover
	rebar3 as test cover --verbose

rel:
	rebar3 as prod release
	rebar3 as prod tar
	rm -rf x
	mkdir x
	./install.sh x 0.8.0
	x/bin/packbeam version

clean:
	rm -rf _build
	rm -fr doc

publish: doc
	rebar3 as publish hex publish --doc-dir docs
