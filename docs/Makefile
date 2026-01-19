# Documentation build targets
# Requires: pandoc, plantuml

MD_FILES := $(wildcard *.md)
PUML_FILES := $(wildcard *.puml)
PDF_FILES := $(MD_FILES:.md=.pdf)
HTML_FILES := $(MD_FILES:.md=.html)
PNG_FILES := $(PUML_FILES:.puml=.png)
SVG_FILES := $(PUML_FILES:.puml=.svg)

# PlantUML settings
PLANTUML_DPI := 300

.PHONY: all clean pdf html png svg

all: pdf html png svg

pdf: $(PDF_FILES)

html: $(HTML_FILES)

png: $(PNG_FILES)

svg: $(SVG_FILES)

%.pdf: %.md
	pandoc $< -o $@ --pdf-engine=xelatex -V geometry:margin=1in

%.html: %.md
	pandoc $< -o $@ --standalone --toc

%.png: %.puml
	plantuml -tpng -Sdpi=$(PLANTUML_DPI) $<

%.svg: %.puml
	plantuml -tsvg $<

clean:
	rm -f $(PDF_FILES) $(HTML_FILES) $(PNG_FILES) $(SVG_FILES)
