.PHONY: build repl test release static help version docker-static docker-image docker-push

.DEFAULT_GOAL = help

VERSION ?= $(shell grep "^version:" pid1.cabal | cut -d " " -f14)
STATIC_BUILD_SCRIPT ?= $(shell nix-build --no-link -A fullBuildScript)
SDIST = $(shell stack sdist --ignore-check 2>&1 | tail -n 1)
LOCAL_USER_ID ?= $(shell id -u $$USER)

## Run build
build:
	@stack build

## Run repl
repl:
	@stack repl

## Run tests.
test:
	@stack test

## Cut new release
release:
	@git tag ${VERSION} && git push --tags

## Build static binary with nix
static:
	@$(STATIC_BUILD_SCRIPT)

## Builds static binary in `out/bin` using fpco/alpine-haskell-stack docker image
docker-static:
	@rm -rf out/bin
	@mkdir -p out/bin
	@mkdir -p out/home
	@docker run --rm \
		-v $(PWD)/out/bin:/host-bin \
		-v ${SDIST}:/sdist.tar.gz \
		-v $(PWD)/out/home:/home/build \
		fpco/alpine-haskell-stack:8.10.4 \
		/bin/bash -c \
		'export HOME=/home/build && chown $$(id -u) $$HOME && rm -rf $$HOME/pid1-* && tar zxfv /sdist.tar.gz && cd pid1-* && stack install --system-ghc --test --local-bin-path /host-bin --ghc-options "-optl-static -fPIC -optc-Os"'

## Build fpco/pid1 docker image (with above built static binary)
docker-image: docker-build-static
	@cp Dockerfile out/bin
	@docker build --tag fpco/pid1:20.04 out/bin
	# Sanity check
	docker run --rm fpco/pid1:20.04 ps

## Push docker image to docker hub
docker-push:
	@docker tag fpco/pid1:20.04 fpco/pid1:latest
	@docker tag fpco/pid1:20.04 fpco/pid1:${VERSION}
	@docker push fpco/pid1:20.04
	@docker push fpco/pid1:${VERSION}
	@docker push fpco/pid1:latest

## Print current version
version:
	@echo ${VERSION}

## Show help screen.
help:
	@echo "Please use \`make <target>' where <target> is one of\n\n"
	@awk '/^[a-zA-Z\-\0-9_]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")-1); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "%-30s %s\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)
