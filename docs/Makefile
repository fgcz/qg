# Documentation build targets
# Requires: pandoc, plantuml

MD_FILES := $(wildcard *.md)
PUML_FILES := $(wildcard *.puml)
PDF_FILES := $(MD_FILES:.md=.pdf)
PNG_FILES := $(PUML_FILES:.puml=.png)
SVG_FILES := $(PUML_FILES:.puml=.svg)

# PlantUML settings
PLANTUML_DPI := 300

.PHONY: all clean pdf png svg

all: pdf png svg

pdf: $(PDF_FILES)

png: $(PNG_FILES)

svg: $(SVG_FILES)

%.pdf: %.md
	pandoc $< -o $@ --pdf-engine=xelatex -V geometry:margin=1in

%.png: %.puml
	plantuml -tpng -Sdpi=$(PLANTUML_DPI) $<

%.svg: %.puml
	plantuml -tsvg $<

clean:
	rm -f $(PDF_FILES) $(PNG_FILES) $(SVG_FILES)
