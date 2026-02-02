.PHONY: build test format lint check clean install uninstall help

all: build

build:
	cabal build

test:
	cabal test --test-show-details=direct

format:
	find src test -name '*.hs' -exec ormolu --mode inplace {} +

format-check:
	find src test -name '*.hs' -exec ormolu --mode check {} +

lint:
	hlint src test

check: format-check lint

clean:
	cabal clean

install:
	cabal install --overwrite-policy=always

uninstall:
ifeq ($(HASKBOX_FORCE_UNINSTALL),1)
	rm -f ~/.cabal/bin/haskbox
else
	@echo "This will remove ~/.cabal/bin/haskbox"
	@read -p "Continue? [y/N] " confirm && [ "$$confirm" = "y" ] && rm -f ~/.cabal/bin/haskbox || echo "Aborted."
endif

help:
	@echo "Available targets:"
	@echo "  build        - Build the project"
	@echo "  test         - Run tests"
	@echo "  format       - Format all source files with ormolu"
	@echo "  format-check - Check formatting without modifying files"
	@echo "  lint         - Run hlint on source files"
	@echo "  check        - Run format-check and lint"
	@echo "  clean        - Clean build artifacts"
	@echo "  install      - Install the binary"
	@echo "  uninstall    - Remove the installed binary (use HASKBOX_FORCE_UNINSTALL=1 to skip confirmation)"
	@echo "  help         - Show this help"
