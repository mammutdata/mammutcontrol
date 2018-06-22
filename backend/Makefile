help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

ghcid-devel: ## Run the server in fast development mode. See MammutControl.DevelServer for details.
	ghcid \
	  --command "hpack; cabal new-repl lib:mammut-control --flag devel" \
	  --restart=package.yaml \
	  --test "MammutControl.DevelServer.reload"

.PHONY: ghcid-devel help
