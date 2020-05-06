
all: webcheck.html webcheck.pdf

clean:
	rm -f webcheck.pdf webcheck.html

%.pdf: %.md
	pandoc \
		-V"date=$(shell date --iso)" \
		--pdf-engine=xelatex \
		--highlight-style=kate \
		$< \
		-o $@

%.html: %.md
	pandoc -s \
		-V"date=$(shell date --iso)" \
		--highlight-style=kate \
		$< \
		-o $@
