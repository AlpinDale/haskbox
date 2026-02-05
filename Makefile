.PHONY: build test format lint check clean install uninstall help \
        platform-test docker-build-linux test-linux test-macos

DOCKER_IMAGE_LINUX := haskbox-test:ubuntu2204
DOCKERFILE_LINUX := test/Dockerfile

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

docker-build-linux:
	@echo "Building Linux Docker image..."
	docker build -f $(DOCKERFILE_LINUX) -t $(DOCKER_IMAGE_LINUX) .

test-linux: docker-build-linux
	@echo "Running tests on Linux (Docker)..."
	docker run --rm -v "$$(pwd)":/workspace $(DOCKER_IMAGE_LINUX) \
		bash -c "cd /workspace && cabal build && cabal test"

test-macos:
	@echo "Running tests on macOS (native)..."
	cabal build
	cabal test

# On macOS: runs native macOS tests + Linux Docker tests
# On Linux: runs native Linux tests only
platform-test:
	@echo "=== Platform Tests ==="
	@UNAME_S=$$(uname -s); \
	if [ "$$UNAME_S" = "Darwin" ]; then \
		echo "Detected macOS - running tests on macOS and Linux"; \
		echo ""; \
		echo ">>> macOS Tests (native) <<<"; \
		$(MAKE) test-macos; \
		MACOS_EXIT=$$?; \
		echo ""; \
		echo ">>> Linux Tests (Docker) <<<"; \
		$(MAKE) test-linux; \
		LINUX_EXIT=$$?; \
		echo ""; \
		echo "=== Platform Test Summary ==="; \
		if [ $$MACOS_EXIT -eq 0 ]; then \
			echo "macOS: PASSED"; \
		else \
			echo "macOS: FAILED"; \
		fi; \
		if [ $$LINUX_EXIT -eq 0 ]; then \
			echo "Linux: PASSED"; \
		else \
			echo "Linux: FAILED"; \
		fi; \
		exit $$(( $$MACOS_EXIT + $$LINUX_EXIT )); \
	elif [ "$$UNAME_S" = "Linux" ]; then \
		echo "Detected Linux - running native tests only"; \
		echo "(macOS tests skipped - cannot run macOS in Docker on Linux)"; \
		echo ""; \
		echo ">>> Linux Tests (native) <<<"; \
		$(MAKE) test; \
	else \
		echo "Unknown platform: $$UNAME_S"; \
		exit 1; \
	fi

help:
	@echo "Available targets:"
	@echo "  build         - Build the project"
	@echo "  test          - Run tests (native platform only)"
	@echo "  format        - Format all source files with ormolu"
	@echo "  format-check  - Check formatting without modifying files"
	@echo "  lint          - Run hlint on source files"
	@echo "  check         - Run format-check and lint"
	@echo "  clean         - Clean build artifacts"
	@echo "  install       - Install the binary"
	@echo "  uninstall     - Remove the installed binary (use HASKBOX_FORCE_UNINSTALL=1 to skip confirmation)"
	@echo ""
	@echo "  platform-test - Run tests on all available platforms"
	@echo "                  On macOS: native + Linux Docker"
	@echo "                  On Linux: native only"
	@echo "  test-linux    - Run tests in Linux Docker container"
	@echo "  test-macos    - Run tests on macOS (native)"
	@echo ""
	@echo "  help          - Show this help"
