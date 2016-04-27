[![Build Status](https://travis-ci.org/ontodev/howl.svg?branch=master)](https://travis-ci.org/ontodev/howl)
[![Clojars Project](https://img.shields.io/clojars/v/howl.svg)](https://clojars.org/howl)

# Humane OWL (HOWL) Format

HOWL makes it easy for humans to read and write [RDF](http://www.w3.org/TR/rdf11-concepts/) and [OWL](http://www.w3.org/TR/owl2-overview/). Here you'll find a description of HOWL, examples, and tools for working with HOWL data.

This is work-in-progress. Your [feedback](http://github.com/ontodev/howl/issues) is appreciated!


## Example

    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX xsd: <http://www.w4.org/2001/XMLSchema#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX obo: <http://purl.obolibrary.org/obo/>
    PREFIX ex: <http://example.com/>

    LABEL rdf:type: type
    LABEL rdfs:label: label
    LABEL rdfs:comment: comment
    LABEL obo:BFO_0000051: has part
    TYPE comment: xsd:string

    ex:ontology
    label: Example Ontology
    type:> owl:Ontology

    ex:foo
    label: Foo
    type:> owl:Class
    comment: A comment on 'Foo'.@en
    > comment: An annotation on the comment.^^xsd:string
    >> comment: An annotation on the annotation.
    comment: Values can span multiple lines,
      and include blank lines...

      as long as each line after the first
      is indented with two spaces.

    ex:bar
    label: Bar
    type:> owl:Class
    subClassOf:>> 'has part' some Bar

    # Lines starting with '#' are just comments.

See the [ontology](ontology) directory and [Makefile](Makefile) for more examples.


## Status

The 0.1 series supports:

- draft HOWL syntax
- basic OWL class expressions from [Protégé](http://protege.stanford.edu):
  `not`, `and`, `or`, `some`, `only`
- converting HOWL to N-Triples, N-Quads, or JSON

You can use another tool such as [rapper](http://librdf.org/raptor/rapper.html) or [Jena riot](https://jena.apache.org/documentation/io/) to convert from N-Triples/N-Quads to any other concrete RDF or OWL syntax.

**Not yet supported:** Converting to HOWL from other RDF and OWL syntaxes.


## Installation and Usage

You can install HOWL as a command-line tool (`howl`) or use it as a Clojure library:


### Command Line Tool

The `howl` command-line tool requires Java (1.6+).

1. [Download a released version of the `howl` file](https://github.com/ontodev/howl/releases), or build it yourself by following the instructions below
2. Rename the file to `howl`, put it on your PATH, and make it executable: `chmod +x howl`
3. Run `howl` on one or more HOWL files to convert them to N-Triples:

```
howl input.howl input2.howl > output.nt
```

Run `howl --help` for more options. The `howl` file is a JAR, so you can also run `java -jar howl`, with standard Java options.


### Clojure Library

HOWL is also available as a Clojure library. See the Clojars page for details:

[![Clojars Project](https://img.shields.io/clojars/v/howl.svg)](https://clojars.org/howl)


## Features

Features in this example:

- `PREFIX ex: <http://example.com/>`
    - SPARQL-style PREFIXes
- `LABEL obo:BFO_0000051: has part`
    - like PREFIXes for single terms
- `TYPE comment: xsd:string`
    - set default language tag or datatype for a predicate
- prefixes, labels, and types
    - can occur at any point in the document
    - can be included from an external document, keeping the main document very simple
    - can be automatically generated using tools (not yet supported)
- `ex:ontology`
    - specify the current subject by its prefixed name, IRI, or label
- `label: Example Ontology`
    - use `: ` for literal statements
    - specify a predicate by its prefixed name, IRI, or label
    - literals quotation marks
    - multi-line literals are indented
    - optionally append a language tag or datatype
- `type:> owl:Ontology`
    - use `:> ` for link statements (when the object is a node)
    - specify the object by its prefixed name, IRI, or label
- `subClassOf:>> 'has part' some Bar`
    - use `:>> ` for expression statements
    - supports Manchester syntax, as used in [Protégé](http://protege.stanford.edu)
- `> comment: An annotation on the comment.^^xsd:string`
    - use `> ` for an OWL annotation
    - use `>> ` for nested annotations (and so on)

Other features, not in this example:

- RDF dataset support: default graph and zero or more named graphs using `GRAPH`
- set or change the `BASE` IRI at any point in the document

Behind the scenes:

- line-based format for stream processing
- JSON format for parse information, for language agnostic tooling

HOWL is designed to be simple and predictable. The most common mistakes will probably be using a label that has not been defined, or using the wrong separator. 

- `: ` for text, numbers, dates, and other literal values
- `:> ` for links
- `:>> ` for complex expressions, i.e. things that link to things

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

There are ten kinds of block:

- Comment
- BASE
- PREFIX
- LABEL
- TYPE
- GRAPH
- Subject
- Literal (a statement in which the object is an RDF literal)
- Link (a statement in which the object is an RDF node)
- Expression (a statement in which the object is an OWL expression)


### Comments

Comment blocks do not specify any information for HOWL, but the parser does keep track of them. A comment line **must** start with '#' -- the '#' has no special meaning unless it is the first character on a line.

This comment block:

    # Just a comment.

is parsed into this JSON object:

    {"file-name": "example.howl",
     "line-number": 1,
     "block": "# Just a comment.\n",
     "block-type": "COMMENT_BLOCK",
     "parse": ["COMMENT_BLOCK"
               "# ",
               "Just a comment."
               ["EOL" "\n"]],
     "hash": "# ",
     "comment": "Just a comment.",
     "eol": "\n"}


### BASE

Base blocks set the current base IRI for resolving relative IRIs. Multiple base blocks can occur, each changing the current base from that point until the next base block.

This prefix block:

    BASE <http://example.com/>

is parsed into this JSON object:

    {"file-name": "example.howl",
     "line-number": 1,
     "block": "BASE <http://example.com/>\n",
     "block-type": "BASE_BLOCK",
     "parse": ["BASE_BLOCK"
               "BASE"
               ["SPACES" " "]
               ["IRI" "<" "http://example.com/" ">"]
               ["EOL" "\n"]],
     "iri": "http://example.com/",
     "eol": "\n"}


### PREFIX

Prefix blocks are identical to SPARQL prefix lines.

This prefix block:

    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

is parsed into this JSON object:

    {"file-name": "example.howl",
     "line-number": 1,
     "block": "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n",
     "block-type": "PREFIX_BLOCK",
     "parse": ["PREFIX_BLOCK"
               "PREFIX"
               ["SPACES" " "]
               ["PREFIX" "rdf"]
               ["COLON" "" ":" " "]
               ["IRI" "<" "http://www.w3.org/1999/02/22-rdf-syntax-ns#" ">"]
               ["EOL" "\n"]],
     "prefix": "rdf",
     "iri": "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
     "eol": "\n"}


### LABEL

HOWL makes RDF more readable by using labels rather than IRIs and prefixed names whenever possible. Label blocks allow you to associate a label to an identifier, without making any other assertions about it -- no triple will be generated. If you want to assert that a subject has a label, use the special `label:` predicate shown below.

This label block:

    LABEL rdfs:comment: comment

is parsed into this JSON object:

    {"file-name": "example.howl",
     "line-number": 1,
     "block": "LABEL rdfs:comment: comment\n",
     "block-type": "LABEL_BLOCK",
     "parse": ["LABEL_BLOCK"
               "LABEL"
               ["SPACES" " "]
               ["IDENTIFIER" ["PREFIXED_NAME" "rdfs" ":" "comment"]]
               ["COLON" "" ":" " "]
               ["LABEL" "comment"]
               ["EOL" "\n"]],
     "identifier": ["PREFIXED_NAME" "rdfs" ":" "comment"],
     "label": "comment",
     "eol": "\n"}


### TYPE

Like JSON-LD, HOWL allows you to associate a default datatype (or language tag) with a predicate. Unless they specify a datatype (or language tag), any literal object for that predicate will use that datatype (or language tag). No triple is generated for a type block.

This type block:

    TYPE comment: xsd:string

is parsed into this JSON object:

    {"file-name": "example.howl",
     "line-number": 1,
     "block": "TYPE comment: xsd:string\n",
     "block-type": "TYPE_BLOCK",
     "parse": ["TYPE_BLOCK"
               "TYPE"
               ["SPACES" " "]
               ["PREDICATE" ["LABEL" "comment"]]
               ["COLON" "" ":" " "]
               ["DATATYPE" ["PREFIXED_NAME" "xsd" ":" "string"]]
               ["EOL" "\n"]],
     "predicate": ["LABEL" "comment"],
     "datatype": ["PREFIXED_NAME" "xsd" ":" "string"],
     "eol": "\n"}


### GRAPH

A HOWL file specifies an RDF dataset, with a default RDF graph and zero or more named RDF graphs. For each block there is a current graph, starting with the default graph, and changed whenever a graph block occurs. Every subject and statement block is assigned to the current graph.

Graph blocks have two forms:

1. `GRAPH IDENTIFIER` specifies a named graph; the IDENTIFIER will also be the subject, so it can be followed by statements about the named graph
2. `GRAPH` specifies the default graph; the default graph cannot be a subject

This graph block:

    GRAPH ex:graph

is parsed into this JSON object:

    {"file-name": "example.howl",
     "line-number": 1,
     "block": "GRAPH ex:graph\n",
     "block-type": "GRAPH_BLOCK",
     "parse": ["GRAPH_BLOCK"
               "GRAPH"
               ["SPACES" " "]
               ["GRAPH" ["PREFIXED_NAME" "ex" ":" "graph"]]
               ["EOL" "\n"]],
     "graph": ["PREFIXED_NAME" "ex" ":" "graph"],
     "eol": "\n"}

This graph block:

    GRAPH

is parsed into this JSON object:

    {"file-name": "example.howl",
     "line-number": 1,
     "block": "GRAPH\n",
     "block-type": "GRAPH_BLOCK",
     "parse": ["GRAPH_BLOCK"
               "GRAPH"
               ["EOL" "\n"]],
     "eol": "\n"}


### Subject

A subject block is just the identifier or label for a subject. It specifies the current 

This subject block:

    ex:subject

is parsed into this JSON object:

    {"file-name": "example.howl",
     "line-number": 1,
     "block": "ex:subject\n",
     "block-type": "SUBJECT_BLOCK",
     "parse": ["SUBJECT_BLOCK"
               ["SUBJECT" ["PREFIXED_NAME" "ex" ":" "subject"]]
               ["EOL" "\n"]],
     "subject": ["PREFIXED_NAME" "ex" ":" "subject"],
     "eol": "\n"}


### Literal

The key difference between the HOWL syntax for literals and the Turtle or NTriples syntax is that HOWL does not require quotation marks. A literal block consists of a predicate, a `: ` (colon and one or more spaces), followed by the literal content, and optionally ending with a language tag or datatype.

This literal block:

    comment: This comment has a datatype.^^xsd:string

is parsed into this JSON object:

    {"file-name": "example.howl",
     "line-number": 1,
     "block": "comment: This comment has a datatype.^^xsd:string\n",
     "block-type": "LITERAL_BLOCK",
     "parse": ["LITERAL_BLOCK"
               ["ARROWS" "" ""]
               ["PREDICATE" ["LABEL" "comment"]]
               ["COLON" "" ":" " "]
               ["LITERAL"
                "This comment has a datatype."
                "^^"
                ["DATATYPE" ["PREFIXED_NAME" "xsd" ":" "string"]]]
               ["EOL" "\n"]],
     "arrows": "",
     "predicate": ["LABEL" "comment"],
     "content": "This comment has a datatype.",
     "datatype": ["PREFIXED_NAME" "xsd" ":" "string"],
     "eol": "\n"}

So these HOWL blocks:

```
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w4.org/2001/XMLSchema#>
PREFIX ex: <http://example.com>
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
"This comment has a datatype."^^<http://www.w4.org/2001/XMLSchema#string> .
```


### Link

To express a triple where the object is an IRI, we use a link block. The subject for the link block will be whatever the current subject is, as specified in a previous subject block or graph block. The link block consists of a predicate, a `:> ` (colon, arrow, and one or more spaces) separator, and an object. The predicate can be a prefixed name, IRI, or label. The object can be any of these, or a blank node.

This link block:

    rdf:type:> owl:Class

is parsed into this JSON object:

    {"file-name": "example.howl",
     "line-number": 1,
     "block": "rdf:type:> owl:Class\n",
     "block-type": "LINK_BLOCK",
     "parse": ["LINK_BLOCK"
               ["ARROWS" "" ""]
               ["PREDICATE" ["PREFIXED_NAME" "rdf" ":" "type"]]
               ["COLON_ARROW" "" ":>" " "]
               ["OBJECT" ["PREFIXED_NAME" "owl" ":" "Class"]]
               ["EOL" "\n"]],
     "arrows": "",
     "predicate": ["PREFIXED_NAME" "rdf" ":" "type"],
     "object": ["PREFIXED_NAME" "owl" ":" "Class"],
     "eol": "\n"}

So these HOWL blocks:

```
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX ex: <http://example.com>
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
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX ex: <http://example.com>
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
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w4.org/2001/XMLSchema#>
PREFIX ex: <http://example.com>
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
"This comment has a datatype."^^<http://www.w4.org/2001/XMLSchema#string> .

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
"This comment has a datatype."^^<http://www.w4.org/2001/XMLSchema#string> .

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

The `howl` tool is written in Clojure. [Leiningen](http://leiningen.org) 2.5+ is required to build it.

- `lein uberjar` builds a standalone JAR file in `target/`
- `lein test` runs the unit and integration tests
- `lein run` can be used to compile and run the command-line interface during development


## Release History

- 0.2.0-SNAPSHOT tweak grammar
  - allow comments
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
