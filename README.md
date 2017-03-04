[![Build Status](https://travis-ci.org/ontodev/howl.svg?branch=master)](https://travis-ci.org/ontodev/howl)
[![Clojars Project](https://img.shields.io/clojars/v/howl.svg)](https://clojars.org/howl)

# HOWL: Humane OWL Format

HOWL makes it easy for humans to read and write [RDF](http://www.w3.org/TR/rdf11-concepts/) and [OWL](http://www.w3.org/TR/owl2-overview/). Here you'll find a description of HOWL, examples, and tools for working with HOWL data.

[**Try HOWL!**](http://try.humaneowl.com)

This is work-in-progress. Your [feedback](http://github.com/ontodev/howl/issues) is appreciated!

## Example

The goal of HOWL is to let you focus on writing the *content* of your dataset or ontology, like this:

    assay
    type: owl:Class
    definition: A planned process with the objective to produce information
      about the material entity that is the evaluant,
      by physically examining it or its proxies.
    equivalent to: 'achieves planned objective' some 'assay objective'

The first line in this example identifies a *subject*, in this case it is the label for class in the OBI ontology. The following lines are statements about the subject, stating its type, textual definition, and logical definition.

By giving this content a *context*, we can convert it to other linked data formats, such RDF XML and Turtle. The context contains:

- a base: letting you use relative IRIs
- prefixes: letting you shorten long IRIs
- labels: letting you replace opaque IRIs with human-friendly labels

The context can be included at the beginning of your file, or stored in a separate file. You can also use labels from an ontology file. Here's an example context for the content above:

    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    LABEL label: rdfs:label
    LABEL type [LINK]: rdf:type
    LABEL definition: obo:IAO_0000115
    LABEL Manchester: <http://www.w3.org/TR/owl2-manchester-syntax/>
    LABEL equivalent to [LINK / Manchester]: owl:equivalentClass
    LABEL assay: obo:OBI_0000070
    LABEL achieves planned objective: obo:OBI_0000417
    LABEL assay objective: obo:OBI_0000441

For more examples, see the [demo site](http://try.humaneowl.com),
the [test](test) and [ontology](ontology) directories,
and the [Makefile](Makefile).


## Status

The [0.3](https://github.com/ontodev/howl/tree/v0.3.0) series supports:

- draft HOWL syntax (some changes from [0.2](https://github.com/ontodev/howl/tree/v0.2.0))
- basic OWL class expressions in Manchester syntax (as used by [Protégé](http://protege.stanford.edu)):
  `not`, `and`, `or`, `some`, `only`
- converting HOWL to N-Triples, N-Quads, or JSON (all versions)
- converting to many other RDF formats (JVM version)

You can use another tool such as [rapper](http://librdf.org/raptor/rapper.html) or [Jena riot](https://jena.apache.org/documentation/io/) to convert from N-Triples/N-Quads to any other concrete RDF or OWL syntax.

**Not yet supported:** Converting to HOWL from other RDF and OWL syntaxes.


## Installation and Usage

You can install HOWL as:

1. a command-line tool (`howl`)
2. a Clojure library
3. a JavaScript library


### 1. Command Line Tool

The `howl` command-line tool requires Java (1.6+). On Unix (Linux, Mac OS X):

1. [Download a released version of the `howl-X.Y.Z.jar` file](https://github.com/ontodev/howl/releases), or build it yourself by following the instructions below
2. Rename the file to `howl`, put it on your PATH, and make it executable: `chmod +x howl`
3. Run `howl` on one or more HOWL files to convert them to N-Triples:

```
howl input.howl input2.howl > output.nt
```

Run `howl --help` for more options. The `howl` file is a JAR, so you can also run `java -jar howl`, with standard Java options.

On Windows, download the JAR file and run it from the Command Prompt using Java (not tested!):

```
java -jar howl.jar input.howl input2.howl > output.nt
```


### 2. Clojure Library

HOWL is also available as a Clojure/ClojureScript library. See the Clojars page for details:

[![Clojars Project](https://img.shields.io/clojars/v/howl.svg)](https://clojars.org/howl)


### 3. JavaScript Library

1. [Download a released version of the `howl-X.Y.Z.js` file](https://github.com/ontodev/howl/releases), or build it yourself by following the instructions below
2. Add a `<script>` tag to your HTML page: `<script src="howl.js" type="text/javascript" charset="utf-8"></script>`
3. Call one of the [API functions](src/howl/api.cljc):

```
var your_quad_string = howl.api.convert_to_quads(your_howl_string);
```

It might also work under Node.js -- let us know!


## Labels

RDF is built up from [IRIs](http://tools.ietf.org/html/rfc3987). IRIs provide a flexible system for using, creating, and reusing a practically unlimited number of globally unique names. But IRIs are often difficult for humans to read and write.

The prefixed names used in Turtle and SPARQL syntax are shorter than full IRIs and are often easier to read and write. We have tools for translating between prefixed names and their full IRIs.

HOWL goes one step further, supporting human-readable labels in almost every place that an IRI can be used. The HOWL tools take care of the translation between labels and the IRIs that they denote. A HOWL label is a string that:

- must not begin or end with whitespace
- must not contain newlines or tabs
- must not begin with a reserved word (case sensitive) followed by a space: BASE, PREFIX, LABEL, TYPE, GRAPH
- must not begin with `#`
- must not begin with `>`
- must not contain `: ` (colon space)
- must not contain certain reserved words: `|`, `/`, `+`, `*`, `?`
- must not end with a colon or square brackets: `:`, `]`

HOWL labels are defined either by `LABEL` blocks. Tools can collect the `rdfs:label`s from a source file and define them as labels for your context (not yet supported).


## Datatypes

There are four kinds of RDF objects: links, plain literals, language literals, and typed literals. For every object, HOWL tracks both the value and the datatype: 'LINK', 'PLAIN', a language tag such as '@en-us', or a datatype IRI such as 'xsd:integer'.

Every HOWL statement specifies a predicate, an object, and a datatype. The default datatype is 'PLAIN', so the object is treated as a plain RDF literal string.

There are many predicates that should always be used with objects of a certain type. For example, the `rdf:type` predicate should always be used with a 'LINK', so you should write:

    rdf:type [LINK]: owl:Class

To avoid writing 'LINK' every time you use the `rdf:type` predicate, you can set the *default datatype* for the `rdf:type` predicate to 'LINK'. You do this with a LABEL:

    LABEL type [LINK]: rdf:type

Then you can simply write:

    type: owl:Class

The default type can also be a language tag, datatype IRI, or a combination of types, such as:

    LABEL equivalent to [LINK / Manchester]: owl:equivalentClass

This allows you to write in Manchester syntax, like Protégé:

    equivalent to: 'achieves planned objective' some 'assay objective'


## Build

The `howl` tool is written in cross-platform Clojure. [Leiningen](http://leiningen.org) 2.5.2+ is required to build it.

- `lein uberjar` builds a standalone JAR file in `target/`
- `lein run` can be used to compile and run the command-line interface during development
- `lein cljsbuild once` builds a JavaScript file in `target/`
- `lein test` runs the unit and integration tests against the JVM
- `lein doo phantom cljs-test` runs the unit test suite against [PhantomJS](http://phantomjs.org) (which must be installed separately)

The [Makefile](Makefile) also contains some convenient build tasks.


## Release History

- 0.3.0 simplify grammar and refactor
  - complete rewrite
  - STATEMENT_BLOCK (using `: `) unifies LITERAL_BLOCK, LINK_BLOCK, and EXPRESSION_BLOCK
  - merge DEFAULT into LABEL, allowing multiple datatypes
  - change PREFIX to match Turtle and SPARQL
  - consistently wrap relative and absolute IRIs in angle brackets
  - major changes to JSON representation
  - more tests and examples
  - add `--context` command-line option
  - changed default output format to N-Quads
- 0.2.0 improve grammar
  - allow comments
  - PREFIX now uses `:> `, for consistency
  - TYPE now uses `:> `, for consistency
  - absolute IRIs don't have to be wrapped in angle brackets
  - allow initial blank lines
- 0.1.1
  - add cross-platform support: Clojure and ClojureScript
  - add cross-platform API in `api.cljc`
  - add `--version` command-line option
  - fix named graph support
  - fix relative IRI resolution
  - fix: TYPE can be language tag
  - refactor render-quads
  - more tests
- 0.1.0 initial release
  - draft HOWL syntax
  - basic OWL class expressions from [Protégé](http://protege.stanford.edu):
    `not`, `and`, `or`, `some`, `only`
  - converting HOWL to N-Triples, N-Quads, or JSON


## License

Copyright © 2016, James A. Overton

Distributed under the Simplified BSD License: [http://opensource.org/licenses/BSD-2-Clause]()
