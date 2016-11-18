[![Build Status](https://travis-ci.org/ontodev/howl.svg?branch=master)](https://travis-ci.org/ontodev/howl)
[![Clojars Project](https://img.shields.io/clojars/v/howl.svg)](https://clojars.org/howl)

# HOWL: Humane OWL Format

HOWL makes it easy for humans to read and write [RDF](http://www.w3.org/TR/rdf11-concepts/) and [OWL](http://www.w3.org/TR/owl2-overview/). Here you'll find a description of HOWL, examples, and tools for working with HOWL data.

[**Try HOWL!**](http://try.humaneowl.com)

This is work-in-progress. Your [feedback](http://github.com/ontodev/howl/issues) is appreciated!

## Example

The first part of the example is a *context* which can be stored in a separate file.

    PREFIXES
      rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      xsd: <http://www.w3.org/2001/XMLSchema#>
      owl: <http://www.w3.org/2002/07/owl#>
      obo: <http://purl.obolibrary.org/obo/>
      ex: <http://example.com/>
    LABELS
      label: rdfs:label
      comment [xsd:string]: rdfs:comment
      type [IRI]: rdf:type
      has part: obo:BFO_0000051
      Manchester Syntax: <http://www.w3.org/TR/owl2-manchester-syntax/>
      subclass of [Manchester Syntax]: rdfs:subClassOf

    ex:ontology
    label: Example Ontology
    type: owl:Ontology

    ex:foo
    label: Foo
    type: owl:Class
    comment [@en]: A comment on 'Foo'.
    > comment: An annotation on the comment.
    >> comment: An annotation on the annotation.
    comment: Values can span multiple lines,
      and include blank lines...

      as long as each line after the first
      is indented with two spaces.

    ex:bar
    label: Bar
    type: owl:Class
    subclass of: 'has part' some Bar

    # Lines starting with '#' are just comments.

For more examples, see the [demo site](http://try.humaneowl.com),
the [ontology](ontology) directory,
and the [Makefile](Makefile).


## Status

The [0.2](https://github.com/ontodev/howl/tree/v0.2.0) series supports:

- draft HOWL syntax (some changes from [0.1](https://github.com/ontodev/howl/tree/v0.1.1))
- basic OWL class expressions from [Protégé](http://protege.stanford.edu):
  `not`, `and`, `or`, `some`, `only`
- converting HOWL to N-Triples, N-Quads, or JSON

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


## Features

Features in this example:

- `PREFIXES`
    - set prefixes, similar to Turtle and SPARQL
- `LABELS`
    - like PREFIXes for single terms
    - set default language tag or datatype for a predicate
- prefixes, labels, and types
    - can be included from an external document, keeping the main document very simple
    - can be automatically generated using tools (not yet supported)
- `ex:ontology`
    - specify the current subject by its prefixed name, IRI, or label
- `label: Example Ontology`
    - make a statement about the subject
    - specify a predicate by its prefixed name, IRI, or label
    - multi-line literals are indented
    - optionally specify the type or language
- `> comment: An annotation on the comment.`
    - make an annotation on a statement
    - use `> ` for an OWL annotation
    - use `>> ` for nested annotations (and so on)

Other features, not in this example:

- RDF dataset support: default graph and zero or more named graphs using `GRAPH`
- set or change the `BASE` IRI at any point in the document

Behind the scenes:

- line-based format for stream processing
- JSON format for parse information, for language agnostic tooling

There are plans for tools that will:

- check for missing labels
- checking for dangerous or misleading labels
- find missing labels among a list of ontologies, and write them to a file
- update labels, using the IRI to replacing an old label with a new label
- "linting" statements to infer whether the user meant to use a literal, link, or expression


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
- must not contain `:> ` (colon arrow space)
- must not contain `:>> ` (colon arrow arrow space)

HOWL labels are defined either by `LABEL` blocks, or when an `rdfs:label` is asserted for a subject and that label meets these criteria.


## Syntax and Parsing

HOWL is build from a sequence of "blocks". Each block is a string consisting of one line of text, followed by zero or more blank or indented lines of text. HOWL is designed for streaming, so a sequence of files is transformed into a sequence of parsed blocks. Each parsed block can be represented as a JSON object.

These are all the block types:

- Blank
- Comment
- BASE
- PREFIXES
- LABELS
- GRAPH
- Subject
- Statement


### Comment

Comment blocks do not specify any information for HOWL, but the parser does keep track of them. A comment line **must** start with '#' -- the '#' has no special meaning unless it is the first character on a line.

This comment block:

    # Just a comment.

is parsed into this JSON object:

    {"source": "example.howl",
     "line": 1,
     "block": "# Just a comment.\n",
     "block-type": "COMMENT_BLOCK",
     "hash": "# ",
     "comment": "Just a comment.",
     "parse-tree":
     ["COMMENT_BLOCK",
      "# ",
      "Just a comment."],
     "leading-whitespace": "",
     "trailing-whitespace": "\n"}


### PREFIXES

This prefix block:

    PREFIXES
      rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

is parsed into this JSON object:

    {"source": "example.howl",
     "line": 1,
     "block": "PREFIXES\nrdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n",
     "block-type": "PREFIXES_BLOCK",
     "parse-tree":
     ["PREFIXES_BLOCK",
      "PREFIXES",
      ["WHITESPACE", "\n"],
      ["PREFIX_LINE",
       ["PREFIX", "rdf"],
       ["COLON", "", ":", " "],
       ["IRIREF", "<", "http://www.w3.org/1999/02/22-rdf-syntax-ns#", ">"]]],
     "prefixes": {
       "rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
     },
     "leading-whitespace": "",
     "trailing-whitespace": "\n"}


### LABELS

HOWL makes RDF more readable by using labels rather than IRIs and prefixed names whenever possible. Label blocks allow you to associate a label to an identifier, without making any other assertions about it -- no triple will be generated. If you want to assert that a subject has a label, use the special `label:` predicate shown below.

This label block:

    LABELS
      comment: rdfs:comment

is parsed into this JSON object:

    {"source": "example.howl",
     "line": 1,
     "block": "LABELS\ncomment: rdfs:comment\n",
     "block-type": "LABELS_BLOCK",
     "parse-tree":
     ["LABELS_BLOCK",
      "LABELS",
      ["WHITESPACE", "\n"],
      ["LABEL_LINE",
       ["LABEL", "comment"],
       ["COLON", "", ":" " "],
       ["PREFIXED_NAME", "rdfs", ":", "comment"]]],
     "labels": {
      "comment": "http://www.w3.org/2000/01/rdf-schema#comment"
     },
     "leading-whitespace": "",
     "trailing-whitespace": "\n"}


### BASE

Base blocks set the current base IRI for resolving relative IRIs. Multiple base blocks can occur, each changing the current base from that point until the next base block.

This prefix block:

    BASE <http://example.com/>

is parsed into this JSON object:

    {"source": "example.howl",
     "line": 1,
     "block": "BASE <http://example.com/>\n",
     "block-type": "BASE_BLOCK",
     "parse-tree":
     ["BASE_BLOCK",
      "BASE",
      ["SPACES", " "],
      ["IRIREF", "<", "http://example.com/", ">"]],
     "base-iri": "http://example.com/",
     "leading-whitespace": "",
     "trailing-whitespace": "\n"}


### GRAPH

A HOWL file specifies an RDF dataset, with a default RDF graph and zero or more named RDF graphs. For each block there is a current graph, starting with the default graph, and changed whenever a graph block occurs. Every subject and statement block is assigned to the current graph.

Graph blocks have two forms:

1. `GRAPH IDENTIFIER` specifies a named graph; the IDENTIFIER will also be the subject, so it can be followed by statements about the named graph
2. `GRAPH` specifies the default graph; the default graph cannot be a subject

This graph block:

    GRAPH ex:graph

is parsed into this JSON object:

    {"source": "example.howl",
     "line": 1,
     "block": "GRAPH ex:graph\n",
     "block-type": "GRAPH_BLOCK",
     "parse-tree":
     ["GRAPH_BLOCK",
      "GRAPH",
      ["SPACES", " "],
      ["PREFIXED_NAME", "ex", ":", "graph"]],
     "graph": ["PREFIXED_NAME", "ex", ":", "graph"],
     "graph-iri": "http://example.com/graph",
     "leading-whitespace": "",
     "trailing-whitespace": "\n"}

This graph block:

    DEFAULT GRAPH

is parsed into this JSON object:

    {"source": "example.howl",
     "line": 1,
     "block": "DEFAULT GRAPH\n",
     "block-type": "GRAPH_BLOCK",
     "parse-tree":
     ["GRAPH_BLOCK"
      "DEFAULT GRAPH"],
     "graph": null,
     "graph-iri": null,
     "leading-whitespace": "",
     "trailing-whitespace": "\n"}


### Subject

A subject block is just the identifier or label for a subject. It specifies the current

This subject block:

    ex:subject

is parsed into this JSON object:

    {"source": "example.howl",
     "line": 1,
     "block": "ex:subject\n",
     "block-type": "SUBJECT_BLOCK",
     "parse-tree":
     ["SUBJECT_BLOCK",
      ["PREFIXED_NAME", "ex", ":", "subject"]],
     "subject": ["PREFIXED_NAME" "ex" ":" "subject"],
     "subject-iri": "http://example.com/subject",
     "leading-whitespace": "",
     "trailing-whitespace": "\n"}


### Statement

The key difference between the HOWL syntax for literals and the Turtle or NTriples syntax is that HOWL does not require quotation marks. A literal block consists of a predicate, a `: ` (colon and one or more spaces), followed by the literal content, and optionally ending with a language tag or datatype.

This literal block:

    comment: This is an RDFS comment.

is parsed into this JSON object:

    {"source": "example.howl",
     "line": 1,
     "block": "comment: This is an RDFS comment.\n",
     "block-type": "STATEMENT_BLOCK",
     "parse-tree":
     ["STATEMENT_BLOCK",
      ["ARROWS", "", ""],
      ["LABEL", "comment"],
      ["DATATYPE"],
      ["COLON", "", ":", " "],
      "This is an RDFS comment."],
     "arrows": "",
     "predicate": ["LABEL", "comment"],
     "datatype": null,
     "content": "This is an RDFS comment.",
     "graph-iri": "http://example.com/current-graph",
     "subject-iri": "http://example.com/current-subject",
     "predicate-iri": "http://www.w3.org/2000/01/rdf-schema#comment",
     "datatype-iri": "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral",
     "language": null,
     "object-iri": null,
     "leading-whitespace": "",
     "trailing-whitespace": "\n"}

This literal block:

    comment [@en]: This is an English comment.

is parsed into this JSON object:

    {"source": "example.howl",
     "line": 1,
     "block": "comment [@en]: This is an English comment.\n",
     "block-type": "STATEMENT_BLOCK",
     "parse-tree":
     ["STATEMENT_BLOCK",
      ["ARROWS", "", ""],
      ["LABEL", "comment"],
      ["DATATYPE", " [", ["LANGUAGE", "@en"], "]"],
      ["COLON", "", ":", " "],
      "This is an English comment."],
     "arrows": "",
     "predicate": ["LABEL", "comment"],
     "datatype": ["LANGUAGE", "@en"],
     "content": "This is an English comment.",
     "graph-iri": "http://example.com/current-graph",
     "subject-iri": "http://example.com/current-subject",
     "predicate-iri": "http://www.w3.org/2000/01/rdf-schema#comment",
     "object-iri": null,
     "datatype-iri": "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral",
     "language": "@en",
     "leading-whitespace": "",
     "trailing-whitespace": "\n"}

This literal block:

    comment [xsd:string]: This comment has a datatype.

is parsed into this JSON object:

    {"source": "example.howl",
     "line": 1,
     "block": "comment [xsd:string]: This comment has a datatype.\n",
     "block-type": "STATEMENT_BLOCK",
     "parse-tree":
     ["STATEMENT_BLOCK",
      ["ARROWS", "", ""],
      ["LABEL", "comment"],
      ["DATATYPE", " [", ["PREFIXED_NAME", "xsd", ":", "string"], "]"],
      ["COLON", "", ":", " "],
      "This comment has a datatype."],
     "arrows": "",
     "predicate": ["LABEL", "comment"],
     "datatype": ["PREFIXED_NAME", "xsd", ":", "string"],
     "content": "This comment has a datatype.",
     "graph-iri": "http://example.com/current-graph",
     "subject-iri": "http://example.com/current-subject",
     "predicate-iri": "http://www.w3.org/2000/01/rdf-schema#comment",
     "object-iri": null,
     "datatype-iri": "http://www.w3.org/2001/XMLSchema#string",
     "language": null,
     "leading-whitespace": "",
     "trailing-whitespace": "\n"}

So these HOWL blocks:

```
PREFIX rdfs:> http://www.w3.org/2000/01/rdf-schema#
PREFIX xsd:> http://www.w3.org/2001/XMLSchema#
PREFIX ex:> http://example.com
LABEL rdfs:comment: comment
GRAPH ex:graph
ex:subject
comment: This comment has a datatype.^^xsd:string
```

specify this NQuad (with newlines added for readability):

```
<http://example.com/graph>
<http://example.com/subject>
<http://www.w3.org/1999/02/22-rdf-syntax-ns#comment>
"This comment has a datatype."^^<http://www.w3.org/2001/XMLSchema#string> .
```


### Link

To express a triple where the object is an IRI, we use a link block. The subject for the link block will be whatever the current subject is, as specified in a previous subject block or graph block. The link block consists of a predicate, a `:> ` (colon, arrow, and one or more spaces) separator, and an object. The predicate can be a prefixed name, IRI, or label. The object can be any of these, or a blank node.

This link block:

    rdf:type [LINK]: owl:Class

is parsed into this JSON object:

    {"source": "example.howl",
     "line": 1,
     "block": "rdf:type [LINK]: owl:Class\n",
     "block-type": "STATEMENT_BLOCK",
     "parse-tree":
     ["STATEMENT_BLOCK",
      ["ARROWS", "", ""],
      ["PREFIXED_NAME", "rdf", ":", "type"]
      ["DATATYPE", " [", "LINK", "]"],
      ["COLON", "", ":", " "],
      ["PREFIXED_NAME", "owl", ":", "Class"]],
     "arrows": "",
     "predicate": ["PREFIXED_NAME", "rdf", ":", "type"],
     "datatype": "LINK",
     "content": ["PREFIXED_NAME", "owl", ":", "Class"],
     "graph-iri": "http://example.com/current-graph",
     "subject-iri": "http://example.com/current-subject",
     "predicate-iri": "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
     "object-iri": "http://www.w3.org/2002/07/owl#Class",
     "datatype-iri": null,
     "language": null,
     "leading-whitespace": "",
     "trailing-whitespace": "\n"}

We can assign default datatypes to labels. When that label is used as a predicate, and no datatype is specified, the default datatype will apply. We have assigned the special type "LINK" to the label "type":

    type: owl:Class

is parsed into this JSON object:

    {"source": "example.howl",
     "line": 1,
     "block": "type: owl:Class\n",
     "block-type": "STATEMENT_BLOCK",
     "parse-tree":
     ["STATEMENT_BLOCK",
      ["ARROWS", "", ""],
      ["LABEL", "type"]
      ["DATATYPE"],
      ["COLON", "", ":", " "],
      ["PREFIXED_NAME", "owl", ":", "Class"]],
     "arrows": "",
     "predicate": ["LABEL", "type"],
     "datatype": null,
     "content": ["PREFIXED_NAME", "owl", ":", "Class"],
     "graph-iri": "http://example.com/current-graph",
     "subject-iri": "http://example.com/current-subject",
     "predicate-iri": "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
     "object-iri": "http://www.w3.org/2002/07/owl#Class",
     "datatype-iri": null,
     "language": null,
     "leading-whitespace": "",
     "trailing-whitespace": "\n"}

So these HOWL blocks:

```
PREFIX rdf:> http://www.w3.org/1999/02/22-rdf-syntax-ns#
PREFIX owl:> http://www.w3.org/2002/07/owl#
PREFIX ex:> http://example.com
GRAPH ex:graph
ex:subject
rdf:type:> owl:Class
```

specify this NQuad (with newlines added for readability):

```
<http://example.com/graph>
<http://example.com/subject>
<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>
<http://www.w3.org/2002/07/owl#Class> .
```

### Expression

HOWL files can also contain OWL class expressions in the version of Manchester syntax familiar from the [Protégé](http://protege.stanford.edu) editor. The subject for the expression block will be whatever the current subject is, as specified in a previous subject block or graph block. The expression block consists of a predicate, a `:>> ` (colon, two arrows, and one or more spaces) separator, and an expression string. The predicate can be a prefixed name, IRI, or label.

This expression block:

    rdfs:subClassOf:>> 'has part' some foo

is parsed into this JSON object:

    TODO
    {"file-name": "example.howl",
     "line-number": 1,
     "block": "rdfs:subClassOf:>> 'has part' some foo\n",
     "block-type": "EXPRESSION_BLOCK",
     "parse": ["EXPRESSION_BLOCK"
               ["PREDICATE" ["PREFIXED_NAME" "rdfs" ":" "subClassOf"]]
               ["COLON_ARROWS" "" ":>>" " "]
               ["MN_CLASS_EXPRESSION"
                ["MN_SOME"
                 ["MN_OBJECT_PROPERTY_EXPRESSION"
                  ["MN_NAME" ["MN_QUOTED_LABEL" "'" "has part" "'"]]]
                 ["MN_SPACE" " "]
                 "some"
                 ["MN_SPACE" " "]
                 ["MN_CLASS_EXPRESSION"
                  ["MN_NAME" ["MN_LABEL" "foo"]]]]]
               ["EOL" "\n"]],
     "predicate": ["PREFIXED_NAME" "rdfs" ":" "subClassOf"],
     "expression": ["MN_CLASS_EXPRESSION"
                    ["MN_SOME"
                     ["MN_OBJECT_PROPERTY_EXPRESSION"
                      ["MN_NAME" ["MN_QUOTED_LABEL" "'" "has part" "'"]]]
                     ["MN_SPACE" " "]
                     "some"
                     ["MN_SPACE" " "]
                     ["MN_CLASS_EXPRESSION"
                      ["MN_NAME" ["MN_LABEL" "foo"]]]]],
     "eol": "\n"}

So these HOWL blocks:

```
PREFIX rdf:> http://www.w3.org/1999/02/22-rdf-syntax-ns#
PREFIX owl:> http://www.w3.org/2002/07/owl#
PREFIX ex:> http://example.com
GRAPH ex:graph
ex:subject
owl:subClassOf:>> 'has part' some foo
```

specify these NTriples (with newlines added for readability):

```
_:b1
<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>
<http://www.w3.org/2002/07/owl#Restriction> .

_:b1
<http://www.w3.org/2002/07/owl#onProperty>
<http://purl.obolibrary.org/obo/BFO_0000051> .

_:b1
<http://www.w3.org/2002/07/owl#someValuesFrom>
<http://example.com/foo> .

<http://example.com/subject>
<http://www.w3.org/2000/01/rdf-schema#subClassOf>
_:b1 .
```


### Annotations

HOWL includes a convenient syntax for OWL annotations: one or more `>` "arrows" preceding a link block or literal block changes the subject to a previous statement.

This literal block:

    > comment: A comment on a comment.

is parsed into this JSON object:

    TODO
    {"file-name": "example.howl",
     "line-number": 1,
     "block": "> comment: A comment on a comment.\n"
     "block-type": "LITERAL_BLOCK",
     "parse": ["LITERAL_BLOCK"
               ["ARROWS" ">" " "]
               ["PREDICATE" ["LABEL" "comment"]]
               ["COLON" "" ":" " "]
               ["LITERAL"
                "A comment on a comment."]
               ["EOL" "\n"]],
     "arrows": ">",
     "predicate": ["LABEL" "comment"],
     "content": "A comment on a comment.",
     "eol": "\n"}

So these HOWL blocks:

```
PREFIX rdf:> http://www.w3.org/1999/02/22-rdf-syntax-ns#
PREFIX owl:> http://www.w3.org/2002/07/owl#
PREFIX ex:> http://example.com
LABEL rdfs:comment: comment
GRAPH ex:graph
ex:subject
comment: This comment has a datatype.^^xsd:string
> comment: A comment on a comment.
```

specify these six NTriples (with newlines added for readability):

1. the target statement
2. the type of annotation: `owl:Axiom`
3. the subject of the target: `owl:annotatedSource`
4. the predicate of the target: `owl:annotatedProperty`
5. the object of the target: `owl:annotatedTarget`
6. the annotation statement

```
<http://example.com/subject>
<http://www.w3.org/1999/02/22-rdf-syntax-ns#comment>
"This comment has a datatype."^^<http://www.w3.org/2001/XMLSchema#string> .

_:b1
<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>
<http://www.w3.org/2002/07/owl#Axiom> .

_:b1
<http://www.w3.org/2002/07/owl#annotatedSource>
<http://example.com/subject> .

_:b1
<http://www.w3.org/2002/07/owl#annotatedProperty>
<http://www.w3.org/1999/02/22-rdf-syntax-ns#comment> .

_:b1
<http://www.w3.org/2002/07/owl#annotatedTarget>
"This comment has a datatype."^^<http://www.w3.org/2001/XMLSchema#string> .

_:b1
<http://www.w3.org/1999/02/22-rdf-syntax-ns#comment>
"A comment on a comment." .
```

Nesting is allowed. The number of arrows specifies the depth of the nesting:

```
comment: A -- original comment
> comment: B -- comment on A
>> comment: C -- comment on B
> comment: D -- another comment on A
```


## Build

The `howl` tool is written in Clojure. [Leiningen](http://leiningen.org) 2.5.2+ is required to build it.

- `lein uberjar` builds a standalone JAR file in `target/`
- `lein cljsbuild once` builds a JavaScript file in `target/`
- `lein test` runs the unit and integration tests
- `lein run` can be used to compile and run the command-line interface during development

The [Makefile](Makefile) also contains some convenient build tasks.


## Release History

- 0.2.1-SNAPSHOT
  - add `--context` command-line option
  - changed default output format to N-Quads
  - rework TYPE and literal representation to match [EDN-LD](http://edn-ld.com)
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
