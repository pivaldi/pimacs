##
# PIMacs
#

.PHONY: all
all: help

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
