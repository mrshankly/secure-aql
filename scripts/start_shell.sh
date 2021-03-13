#!/usr/bin/env bash

set -e

if [ -z "${NODE_NAME}" ]; then
	export NODE_NAME='aql@127.0.0.1'
fi

if [ -z "${AQL_REL}" ]; then
	# by default, this script will be executed from the Makefile
	export AQL_REL=_build/default/rel/aql
fi

echo "Using AQL node name: ${NODE_NAME} (use Ctrl+D to exit)"

${AQL_REL}/bin/env daemon
sleep 5

echo 'aql:start_shell().' | ${AQL_REL}/bin/env daemon_attach >/dev/null 2>&1
sleep 1

if [ $# -eq 0 ]; then
  ${AQL_REL}/bin/env daemon_attach 2>/dev/null
else
  cat "${1}" | tr -d "\r\n" | sed -e 's/;/;\n/g' | ${AQL_REL}/bin/env daemon_attach 2>/dev/null
fi

${AQL_REL}/bin/env stop
