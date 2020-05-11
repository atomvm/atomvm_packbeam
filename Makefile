##
## Copyright (c) dushin.net
## All rights reserved.
##

all: compile escript edoc

compile:
	rebar3 compile

escript:
	rebar3 escriptize

edoc:
	rebar3 edoc
