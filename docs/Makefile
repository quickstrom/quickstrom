PANDOC_OPTS=-V"date=$(shell date --iso)" \
						--highlight-style=kate \
						--toc

all: quickstrom.html quickstrom.pdf

clean:
	rm -rf quickstrom.pdf quickstrom.html

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

.PHONY: all build clean watch