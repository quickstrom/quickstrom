TS_SOURCES=$(shell find lib -name "*.ts")
PANDOC_OPTS=-V"date=$(shell date --iso)" \
						--highlight-style=kate \
						--toc

all: webcheck.html webcheck.pdf build

build: target/webcheck-bundle.js

clean:
	rm -rf webcheck.pdf webcheck.html target

target/webcheck.js: $(TS_SOURCES)
	tsc

target/webcheck-bundle.js: target/webcheck.js
	browserify -s webcheck target/webcheck.js -o target/webcheck-bundle.js

watch:
	echo $(TS_SOURCES) | entr make target/webcheck-bundle.js

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
