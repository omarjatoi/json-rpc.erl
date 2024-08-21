.PHONY: *

default:
	@echo ""
	@echo "Run tasks for json-rpc"
	@echo ""
	@echo "  build     compile the json-rpc application"
	@echo "  clean     run rebar3 clean and delete the build dir"
	@echo "  deps      get dependencies for the project"
	@echo "  format    run the erlfmt formatter"
	@echo "  lint      run linter (rebar3_lint)"
	@echo "  test      run all eunit tests"
	@echo "  sh        launch a nix shell with zsh (erlang, rebar3)"
	@echo ""

build:
	rebar3 compile

clean:
	rebar3 clean
	rm -rf _build

deps:
	rebar3 update

format:
	erlfmt -w

lint:
	rebar3 lint

test:
	rebar3 eunit

sh:
	nix develop --command /bin/zsh
