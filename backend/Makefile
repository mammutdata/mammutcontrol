ifeq (${SLOW}, 1)
slow_flag = --flag slow
endif

help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

ghcid-devel: ## Run the server in fast development mode. See MammutControl.DevelServer for details.
	ghcid \
	  --command "hpack; cabal new-repl lib:mammut-control --flag devel ${slow_flag}" \
	  --restart=package.yaml \
	  --test "MammutControl.DevelServer.reload"

ghcid-test: ## Run the tests repeatedly after each change.
	ghcid \
	  --command "hpack; cabal new-repl unit-tests" \
	  --restart=package.yaml --restart=src -W \
	  --test "Main.ghcidMain"

.PHONY: ghcid-devel help
