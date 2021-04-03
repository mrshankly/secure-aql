TEST_LOGS = _build/test/logs
NODE_NAME = 'aql@127.0.0.1'
COOKIE    = antidote

.SUFFIXES:
.SECONDARY:
.PHONY: all compile release clean shell aqlshell test ct

all: compile release

compile:
	rebar3 compile

release:
	rebar3 release -n aql

clean:
	rm -rf _build/default/rel/
	rebar3 clean

shell: release
	./scripts/rebar_shell.sh

aqlshell: release
	./scripts/start_shell.sh

test:
	rebar3 eunit

ct:
	mkdir -p $(TEST_LOGS)
	./scripts/run_ct.sh $(TEST_LOGS) $(NODE_NAME) $(COOKIE)
