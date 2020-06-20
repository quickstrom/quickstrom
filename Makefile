PANDOC_OPTS=-V"date=$(shell date --iso)" \
						--highlight-style=kate \
						--toc

all: webcheck.html webcheck.pdf

clean:
	rm -rf webcheck.pdf webcheck.html

%.pdf: %.md
	pandoc \
		$(PANDOC_OPTS) \
		--pdf-engine=xelatex \
		$< \
		-o $@

%.html: %.md
	pandoc -s \
		$(PANDOC_OPTS) \
		$< \
		-o $@

push-cachix:
	nix-build -A webcheck --no-out-link | cachix push webcheck
	nix-store -qR --include-outputs $(nix-instantiate shell.nix) | cachix push webcheck

.PHONY: all build clean watch push-cachix