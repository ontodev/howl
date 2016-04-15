# Humane OWL Format (HOWL)

HOWL is (an idea for) a new concrete syntax for RDF and OWL, designed for human editing and standard version control workflows.


## Example

    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX xsd: <http://www.w4.org/2001/XMLSchema#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX ex: <http://example.com/>

    LABEL rdf:type: type
    LABEL rdfs:label: label
    LABEL rdfs:comment: comment
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
    subClassOf:> 'has part' some Bar

    ex:bar
    label: Bar
    type> Class


## Features

Features in this example:

- line 1: SPARQL-style PREFIXes
- line 7: LABELs -- like PREFIXes for single terms
- line 10: TYPEs to set default language tag or datatype for a predicate
- line 12: specify the current subject by its prefixed name, IRI, or label
- line 13:
  - specify a predicate by its prefixed name, IRI, or label
  - use ":" for literal statements
  - literals without quotation marks
- line 14:
  - use ":>" for link statements
  - specify an object by its prefixed name, IRI, or label
- line 20: OWL annotations start with `>`
- line 21: OWL annotations can be nested
- line 22: multi-line values using indentation
- line 27: class expressions use Manchester Syntax and labels

Other features, not in this example:

- RDF dataset support for a default graph and zero or more named graphs
- BASE IRI

Behind the scenes:

- line-based format for stream processing
- JSON format for parse information, for language agnostic tooling


## Goals

For our current purpose, a "humane" syntax is still a formal language suited for machines to read, but designed for humans to read and write in a basic text editor. Syntax highlighting and auto-complete are handy but should not be necessary. It should:

1. have a simple, predictable structure
2. require minimal use of quotations, parentheses, line-terminators, and other "noise"
3. use human-readable labels rather than opaque IDs wherever possible

For bonus points, it should have a deterministic serialization.


## Comparisons

- RDF/XML is used for most OWL files; XML has some virtues as an exchange format, but it is inhumane
- Turtle is the most humane concrete RDF syntax, but still requires quotations, and does not support OWL constructs
- Manchester Syntax (OMN) is the most humane of the concrete OWL syntaxes, but focused on logical structure rather than annotations, and somewhat complex
- OBO Format is humane and close to what we want, but not as expressive as OWL and is now deprecated
- YAML is humane and general-purpose, but it has a big specification and will require quoting for some of our main cases (i.e. key strings with spaces)


Format  | 1 | 2 | 3
--------|---|---|---
RDF/XML | N | N | N
Turtle  | N | N | N
OMN     | N | N | N
OBO     | Y | Y | N
YAML    | Y | Y | ?
HOWL    | Y | Y | Y


## Elements

The first step is to represent RDF in HOWL. There are three elements in RDF:

- IRIs
    - absolute
    - relative
- blank nodes
- literals
    - plain
    - typed
    - language

Since IRIs are cumbersome, Turtle introduces prefixes and QNames. You can use a QName in any place you can use an IRI.

Since many QNames and IRIs are not easy for human to read and remember, we introduce the HLabel. In HOWL syntax you can use an HLabel anywhere you can use an IRI. HLabels are strings that:

- do not begin or end with whitespace
- do not contain newlines or tabs
- do not begin with a reserved word (case sensitive): BASE, PREFIX, LABEL, TYPE, GRAPH
- do not begin with `>`
- do not contain `: ` (colon space)
- do not contain `:> ` (colon arrow space)

HLabels are defined by the rdfs:label predicate. When multiple rdfs:labels are available, HOWL uses the first valid HLabel in a alphanumeric sort of their lexical values.

Because rdfs:labels can overlap and be ambiguous, we introduce the QLabel. A QLabel consists of a prefix (as for a QName) and an HLabel. Example: `OBI:assay`.

A HOWL serializer will refer to a named resource using (in order of preference):

1. an HLabel if the resources has an appropriate, unambiguous rdfs:label string
2. a QLabel if the resource has an appropriate rdfs:label
3. a CURIE: using the longest matching prefix
4. an IRI

A HOWL parser works in two passes, first collecting a map between IDs and HLabels (similar to a JSON-LD context), then parsing the rest of the content. A HOWL parser will handle a resource name as:

1. a blank node if it starts with `_:`
2. an IRI if it starts with `<` and ends with `>`
3. a QLabel if it contains a colon and the HLabel can be found
4. a CURIE if it contains a colon
5. an HLabel if it can be found
6. a name error

A HOWL parser will handle a literal value as:

1. a typed literal if it ends with a type
2. a language literal if it ends with a language tag
3. a plain literal


## HOWL for RDF

HOWL is a line-based format. There are six types of line:

1. `PREFIX` (SPARQL-style)
2. `GRAPH` begins a named graph; not used for OWL, but might be useful
3. `DECLARE` used to start a new subject block
  - `DECLARE ANYID: LABEL` declares a subject and assigns an rdfs:label
  - `DECLARE: LABEL` declares a subject and assigns an rdfs:label; the ID must be given elsewhere
4. assertion: `NAME> NAME` asserts a predicate and resource object for the current subject
  - separated by `> `
5. assertion: `NAME: VALUE` asserts a predicate and literal object for the current subject
  - separated by `: `
6 indented multi-line values: values may continue on consecutive lines
  - lines after the first must be indented by two spaces
  - the multi-line value ends at the EOF or first non-indented line
  - tailing blank lines are stripped unless they are indented

HOWL has a deterministic, idempotent serialization order:

1. PREFIX lines: lexical order
2. GRAPH blocks: lexical order
3. DECLARE blocks: lexical order
4. assertions within DECLARE blocks: lexical order, configurable; priority for rdf:type, rdfs:label


## HOWL for OWL

- class expressions: Use Manchester Syntax for class expressions
- annotated axioms: annotate the previous line
- sort order:
  - OWL types: Ontology, AnnotationProperty, ObjectProperty, Class, Individuals (all others)
  - within these types, as above


## Syntax and Parsing

Each block consists of a line of text, followed by zero or more indented lines. The block is the level at which HOWL is parsed. HOWL is designed for streaming, so a sequence of files is transformed into a sequence of parsed blocks. Each parsed block can be represented as a JSON object.


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

The key difference between the HOWL syntax for literals and the Turtle or NTriples syntax is that HOWL does not require quotation marks. A literal block consists of a predicate, a colon and one or more spaces, followed by the literal content, and optionally ending with a language tag or datatype.

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

To express a triple where the object is an IRI, we use a link block. The subject for the link block will be whatever the current subject is, as specified in a previous subject block or graph block. The link block consists of a predicate, a `:>` "arrow colon" separator, and an object. The predicate can be a prefixed name, IRI, or label. The object can be any of these, or a blank node.

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
               ["ARROW_COLON" "" ":>" " "]
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

HOWL files can also contain OWL class expressions in the version of Manchester syntax familiar from the Protege editor. The subject for the expression block will be whatever the current subject is, as specified in a previous subject block or graph block. The expression block consists of a predicate, a `:>>` "double arrow colon" separator, and an expression string. The predicate can be a prefixed name, IRI, or label.

This expression block:

    rdfs:subClassOf:>> 'has part' some foo

is parsed into this JSON object:

    {"file-name": "example.howl",
     "line-number": 1,
     "block": "rdfs:subClassOf:>> 'has part' some foo\n",
     "block-type": "EXPRESSION_BLOCK",
     "parse": ["EXPRESSION_BLOCK"
               ["PREDICATE" ["PREFIXED_NAME" "rdfs" ":" "subClassOf"]]
               ["ARROWS_COLON" "" ":>>" " "]
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



