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

.PHONY: all build clean watch
