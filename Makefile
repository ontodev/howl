### Configuration
#
# These are standard options to make Make sane:
# <http://clarkgrubb.com/makefile-style-guide#toc2>

MAKEFLAGS += --warn-undefined-variables
SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c
.DEFAULT_GOAL := all
.DELETE_ON_ERROR:
.SUFFIXES:
.SECONDARY:


### Build Binary

build:
	mkdir -p $@

VERSION := $(shell lein project-version)

target/howl-$(VERSION)-standalone.jar: project.clj src
	lein uberjar

build/howl-$(VERSION).jar: src/stub.sh target/howl-$(VERSION)-standalone.jar | build
	cat $^ > $@
	chmod +x $@

target/howl.js: project.clj src
	lein cljsbuild once

build/howl-$(VERSION).js: target/howl.js | build
	cp $< $@


### General Conversion

build/%.nt: build/%.howl | build
	lein run $< > $@

build/%.ttl: build/%.nt
	rapper --input ntriples \
	--output turtle \
	-f 'xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"' \
	-f 'xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"' \
	-f 'xmlns:xsd="http://www.w4.org/2001/XMLSchema#"' \
	-f 'xmlns:owl="http://www.w3.org/2002/07/owl#"' \
	$< > $@

build/%.owl: build/%.ttl
	robot convert --input $< --output $@


### Experiments

# Build a small version of OBI Core
build/obi_core.howl: ontology/prefixes.howl ontology/labels.howl build/metadata.howl build/terms.howl build/core.howl
	cat $^ > $@

build/metadata.howl: | build
	echo 'obo:obi/obi_core.owl' > $@
	echo 'type:> owl:Ontology' >> $@
	echo 'owl:versionIRI:> obo:obi/$(shell date "+%Y-%m-%d")/obi_core.owl' >> $@
	echo '' >> $@

build/test1.nt: test/test1.howl | build
	lein run $< > $@

build/core.howl: ontology/core.md | build
	< $< \
	grep '^    ' \
	| sed 's/^    //' \
	> $@

build/terms.howl: ontology/template.py ontology/terms.tsv | build
	$^ > $@


.PHONY: jar
jar: build/howl-$(VERSION).jar

.PHONY: js
js: build/howl-$(VERSION).js

.PHONY: all
all: jar js

.PHONY: clean
clean:
	rm -rf build target

