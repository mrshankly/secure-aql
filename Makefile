AQL = ./_build/default/lib/aql
AQL_REL = ./_build/default/rel/aql
NODE_NAME = 'aql@127.0.0.1'
NODE_DEV_NAME = 'aqldev@127.0.0.1'
COOKIE = antidote
MAIN = "aql:start_shell()"
EBIN = ./_build/default/lib/*/ebin
TEST_LOGS = _build/test/logs
SCRIPTS = ./scripts

.PHONY: all test clean antidote

all: compile

shell: release
	chmod u+x $(SCRIPTS)/rebar_shell.sh; sync
	$(SCRIPTS)/rebar_shell.sh

aqlshell: release
	chmod u+x $(SCRIPTS)/start_shell.sh; sync
	$(SCRIPTS)/start_shell.sh

dev_public:
	chmod u+x $(SCRIPTS)/start_dev_public.sh; sync
	$(SCRIPTS)/start_dev_public.sh

shell_public:
	chmod u+x $(SCRIPTS)/start_shell_public.sh; sync
	$(SCRIPTS)/start_shell_public.sh

dev:
	chmod +x $(SCRIPTS)/start_dev.sh; sync
	$(SCRIPTS)/start_dev.sh

compile:
	rebar3 compile
	mkdir -p _build/test/logs
	
clean:
	rebar3 clean
	
release:
	rebar3 release

relclean:
	rm -rf _build/default/rel

test:
	rebar3 eunit --cover
	rebar3 cover

ct:
	chmod +x $(SCRIPTS)/run_ct.sh; sync
	$(SCRIPTS)/run_ct.sh $(TEST_LOGS) $(NODE_NAME) $(COOKIE)

dialyzer:
	rebar3 dialyzer
