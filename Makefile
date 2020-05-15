TS_SOURCES=$(shell find lib -name "*.ts")
PANDOC_OPTS=-V"date=$(shell date --iso)" \
						--highlight-style=kate \
						--toc

all: webcheck.html webcheck.pdf build

build: target/index.js

clean:
	rm -rf webcheck.pdf webcheck.html target

target/wtp.js: $(TS_SOURCES)
	tsc --incremental

target/index.js: target/wtp.js
	browserify -s wtp target/wtp.js -o target/index.js

watch:
	echo $(TS_SOURCES) | entr make target/index.js

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
