deps: ## Install the dependencies of the backend
	@cabal build --only-dependencies

build: ## Build the project in fast mode
	@cabal build

clean: ## Remove compilation artifacts
	@cabal clean

repl: ## Start a REPL
	@cabal repl

test: ## Run the test suite
	@cabal test

lint: ## Run the code linter (HLint)
	@find test src -name "*.hs" | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

style: ## Run the code styler (fourmolu and cabal-fmt)
	@fourmolu -q --mode inplace test src
	@cabal-fmt -i *.cabal

check:
	@./scripts/ci-check.sh

nix-check:
	@nix-shell --run "./scripts/ci-check.sh" ./scripts/shell.nix
help: ## Display this help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

PROCS := $(shell nproc)

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
