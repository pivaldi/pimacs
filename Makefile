##
# PIMacs
#

.PHONY: all
all: help

.PHONY: docker-build
docker-build: ## Build PIMacs Docker image.
	docker build -t pimacs:latest -f docker/Dockerfile .

.PHONY: docker-rebuild
docker-rebuild: ## ReBuild PIMacs Docker image disabling cache.
	docker build --no-cache -t pimacs:latest -f docker/Dockerfile .

.PHONY: docker-run
docker-run: ## Run PIMacs in Docker with X11 GUI.
	./docker/run.sh

.PHONY: docker-run-nw
docker-run-nw: ## Run PIMacs in Docker in terminal mode.
	./docker/run.sh -nw

.PHONY: generate-keys-refcards
generate-keys-refcards: ## Generate all the key bindings refcards.
	emacsclient --eval '(pim-generate-all-keymaps) (pim-generate-all-modules-key-bindings-refcards) (pim-generate-all-fundamental-key-bindings)'

.PHONY: help
help: ## Prints this help
	@grep -h -P '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		column -t -s ':' -o ':' | \
		sort | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36mmake  %s\033[0m : %s\n", $$1, $$2}'

# end
