#!/usr/bin/env bash

if [ -z "$NODE_NAME" ]; then
	export NODE_NAME='aql@127.0.0.1'
fi

if [ -z "$AQL_REL" ]; then
	# by default, this script will be executed from the Makefile
	export AQL_REL=_build/default/rel/aql
fi

# echo "Using AQL node name: $NODE_NAME"

$AQL_REL/bin/env daemon && sleep 5 && $AQL_REL/bin/env eval "aql:start_shell()."
