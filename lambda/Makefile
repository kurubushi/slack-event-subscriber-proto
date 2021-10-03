.PHONY: deploy

FUNCTION_NAME := bot
CABAL_DOCKER_IMAGE := cabal-on-al2
CABAL := docker run --rm -v $(PWD):/code -v $(PWD)/tmp/cabal:/root/.cabal $(CABAL_DOCKER_IMAGE) --builddir=/root/.cabal/build

deploy: bootstrap.zip
	aws lambda update-function-code --function-name $(FUNCTION_NAME) --zip-file 'fileb://bootstrap.zip'

%.zip: %
	zip $@ $<

bootstrap: app/*.hs src/*/*.hs .cabal.touch
	rm -f bootstrap || true
	$(CABAL) update
	$(CABAL) --installdir=. --install-method=copy install $@

.cabal.touch: docker/cabal-on-al2/Dockerfile
	docker build -t $(CABAL_DOCKER_IMAGE) -f $< .
	touch $@

builder:
	docker build -t "haskell-lambda-builder" -f Dockerfile.build .
	docker run --name haskell-lambda-builder -v $(PWD):/code haskell-lambda-builder

fmt:
	stylish-haskell -ri app src
	cabal-fmt lambda.cabal -i
