.PHONY: *

default:
	@echo ""
	@echo "Run tasks for json-rpc"
	@echo ""
	@echo "  build     compile the json-rpc application"
	@echo "  format    run the rebar3_format formatter"
	@echo "  test      run all eunit tests"
	@echo "  sh        launch a nix shell with zsh (erlang, rebar3)"
	@echo ""

build:
	rebar3 compile

format:
	rebar3 format

test:
	rebar3 eunit

sh:
	nix develop --command /bin/zsh
