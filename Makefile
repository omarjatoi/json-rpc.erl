.PHONY: *

default:
	@echo ""
	@echo "Run tasks for json-rpc"
	@echo ""
	@echo "  build      compile the json-rpc application"
	@echo "  clean      run rebar3 clean and delete the build dir"
	@echo "  deps       get dependencies for the project"
	@echo "  format     run the erlfmt formatter"
	@echo "  lint       run linter (rebar3_lint)"
	@echo "  xref       run rebar3 xref"
	@echo "  dialyzer   run rebar3 dialyzer"
	@echo "  check      run format, lint, xref, dialyzer (fail fast)"
	@echo "  test       run all common_test suites"
	@echo "  ct         run rebar3 ct"
	@echo "  sh         launch a nix shell with zsh (erlang, rebar3)"
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

xref:
	rebar3 xref

dialyzer:
	rebar3 dialyzer

check: format lint xref dialyzer

test: ct

ct:
	rebar3 ct

sh:
	nix develop --command /bin/zsh
