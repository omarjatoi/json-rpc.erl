.PHONY: default build clean deps format format-check lint xref dialyzer cover docs check test ct sh

default:
	@echo ""
	@echo "Run tasks for json-rpc"
	@echo ""
	@echo "  build         compile the json-rpc application"
	@echo "  clean         run rebar3 clean and delete the build dir"
	@echo "  deps          fetch dependencies"
	@echo "  format        rewrite sources with erlfmt"
	@echo "  format-check  fail if any source is not erlfmt-clean"
	@echo "  lint          run the elvis linter"
	@echo "  xref          run cross-reference analysis"
	@echo "  dialyzer      run the static type analyzer"
	@echo "  test          run all common_test suites"
	@echo "  cover         run the suites and report coverage"
	@echo "  docs          build the ex_doc documentation"
	@echo "  check         format-check, lint, xref, dialyzer, test (fail fast)"
	@echo "  sh            launch a nix shell with zsh"
	@echo ""

SOURCES := src/*.erl test/*.erl include/*.hrl

build:
	rebar3 compile

clean:
	rebar3 clean
	rm -rf _build

deps:
	rebar3 update

format:
	erlfmt -w $(SOURCES)

format-check:
	erlfmt -c $(SOURCES)

lint:
	rebar3 lint

xref:
	rebar3 xref

dialyzer:
	rebar3 dialyzer

test: ct

ct:
	rebar3 ct

cover:
	rebar3 ct --cover
	rebar3 cover --verbose

docs:
	rebar3 ex_doc

check: format-check lint xref dialyzer test

sh:
	nix develop --command /bin/zsh
