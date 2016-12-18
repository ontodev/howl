## Syntax and Parsing

HOWL is built from a sequence of "blocks". Each block is a string consisting of one line of text, followed by zero or lines that are either blank or indented with two spaces. HOWL is designed for streaming, so a sequence of files is transformed into a sequence of parsed blocks. Each parsed block can be represented as a JSON object.

There are just seven types of block:

- Comment
- BASE
- PREFIX
- LABEL
- GRAPH
- Subject
- Statement

The following sections show examples of each block type, along with the JSON representation.


### Comment

Comment blocks do not specify any information for HOWL, but the parser does keep track of them. Any line with '#' as the first character is a comment. The '#' has no special meaning unless it is the first character on a line.

This comment block:

    # Just a comment.

is parsed into this JSON object:

    {"block-type": "COMMENT_BLOCK",
     "source": "example.howl",
     "line": 1,
     "string": "# Just a comment.\n",
     "parse-tree":
     ["COMMENT_BLOCK",
      "# Just a comment."],
     "leading-whitespace": "",
     "trailing-whitespace": "\n",
     "comment": "# Just a comment."}


### PREFIX

Prefix blocks let you abbreviate IRIs by associating a short prefix with a longer IRI. It works just like Turtle and SPARQL. This prefix block:

    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

is parsed into this JSON object:

    {"block-type": "PREFIX_BLOCK",
     "source": "example.howl",
     "line": 1,
     "string": "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n",
     "leading-whitespace": "",
     "trailing-whitespace": "\n",
     "parse-tree":
     ["PREFIX_BLOCK",
      "PREFIX",
      ["SPACES", " "],
      ["PREFIX", "rdf"],
      ["COLON", "", ":", " "],
      ["IRIREF", "<", "http://www.w3.org/1999/02/22-rdf-syntax-ns#", ">"]],
     "prefix": "rdf",
     "iri": "http://www.w3.org/1999/02/22-rdf-syntax-ns#"}


### LABEL

HOWL makes RDF more readable by using labels rather than IRIs and prefixed names whenever possible. Label blocks allow you to associate a label with an IRI. This label block:

    LABEL comment: rdfs:comment

is parsed into this JSON object:

    {"block-type": "LABEL_BLOCK",
     "source": "example.howl",
     "line": 1,
     "string": "LABEL comment: rdfs:comment\n",
     "parse-tree":
     ["LABEL_BLOCK",
      "LABEL",
      ["SPACES", " "],
      ["LABEL", "comment"],
      ["DATATYPES"],
      ["COLON", "", ":" " "],
      ["PREFIXED_NAME", "rdfs", ":", "comment"]],
     "leading-whitespace": "",
     "trailing-whitespace": "\n",
     "label": "comment",
     "datatype-names": [],
     "target-name": ["PREFIXED_NAME", "rdfs", ":", "comment"],
     "iri": "http://www.w3.org/2000/01/rdf-schema#comment",
     "datatypes": []}

Label blocks can also specify a default datatype, that will apply to all following statements that use that predicate and do not specify a datatype. This label block:

    LABEL comment [@en]: rdfs:comment

is parsed into this JSON object:

    {"block-type": "LABEL_BLOCK",
     "source": "example.howl",
     "line": 1,
     "string": "LABEL comment [@en]: rdfs:comment\n",
     "parse-tree":
     ["LABEL_BLOCK",
      "LABEL",
      ["SPACES", " "],
      ["LABEL", "comment"],
      ["DATATYPES", " [", ["LANGUAGE_TAG", "@", "en"], "]"],
      ["COLON", "", ":" " "],
      ["PREFIXED_NAME", "rdfs", ":", "comment"]],
     "leading-whitespace": "",
     "trailing-whitespace": "\n",
     "label": "comment",
     "datatype-names": [["LANGUAGE_TAG", "@", "en"]],
     "target-name": ["PREFIXED_NAME", "rdfs", ":", "comment"],
     "iri": "http://www.w3.org/2000/01/rdf-schema#comment",
     "datatypes": ["@en"]}


### BASE

Base blocks set the current base IRI used to resolve relative IRIs. Multiple base blocks can occur, each changing the current base from that point until the next base block.

This base block:

    BASE <http://example.com/>

is parsed into this JSON object:

    {"block-type": "BASE_BLOCK",
     "source": "example.howl",
     "line": 1,
     "string": "BASE <http://example.com/>\n",
     "parse-tree":
     ["BASE_BLOCK",
      "BASE",
      ["SPACES", " "],
      ["IRIREF", "<", "http://example.com/", ">"]],
     "leading-whitespace": "",
     "trailing-whitespace": "\n",
     "base": "http://example.com/"}


### GRAPH

A HOWL file specifies an RDF dataset, with a default RDF graph and zero or more named RDF graphs. For each block there is a current graph, starting with the default graph, and changed whenever a graph block occurs. Every subject and statement block is assigned to the current graph.

Graph blocks have two forms:

1. `GRAPH name` specifies a named graph; the name will also be the subject, so it can be followed by statements about the named graph
2. `DEFAULT GRAPH` specifies the default graph; the default graph cannot be a subject

This graph block:

    GRAPH ex:graph

is parsed into this JSON object:

    {"block-type": "GRAPH_BLOCK",
     "source": "example.howl",
     "line": 1,
     "string": "GRAPH ex:graph\n",
     "parse-tree":
     ["GRAPH_BLOCK",
      "GRAPH",
      ["SPACES", " "],
      ["PREFIXED_NAME", "ex", ":", "graph"]],
     "leading-whitespace": "",
     "trailing-whitespace": "\n",
     "graph-name": ["PREFIXED_NAME", "ex", ":", "graph"],
     "graph": "http://example.com/graph",
     "subject": "http://example.com/graph"}

This graph block:

    DEFAULT GRAPH

is parsed into this JSON object:

    {"block-type": "GRAPH_BLOCK",
     "source": "example.howl",
     "line": 1,
     "string": "DEFAULT GRAPH\n",
     "parse-tree":
     ["GRAPH_BLOCK"
      "DEFAULT GRAPH"],
     "leading-whitespace": "",
     "trailing-whitespace": "\n",
     "graph-name": null,
     "graph": null,
     "subject": null}


### Subject

A subject block is just the identifier or label for a subject. It specifies the current subject, used for all the statements that follow, until the next subject block. This subject block:

    ex:subject

is parsed into this JSON object:

    {"block-type": "SUBJECT_BLOCK",
     "source": "example.howl",
     "line": 1,
     "string": "ex:subject\n",
     "parse-tree":
     ["SUBJECT_BLOCK",
      ["PREFIXED_NAME", "ex", ":", "subject"]],
     "leading-whitespace": "",
     "trailing-whitespace": "\n",
     "subject-name": ["PREFIXED_NAME" "ex" ":" "subject"],
     "subject": "http://example.com/subject"}


### Statement

The key difference between the HOWL syntax for literals and the Turtle or NTriples syntax is that HOWL does not require quotation marks. A literal block consists of a predicate, a `: ` (colon and one or more spaces), followed by the literal content, and optionally ending with a language tag or datatype.

This statement block:

    comment: This is an RDFS comment.

is parsed into this JSON object:

    {"block-type": "STATEMENT_BLOCK",
     "source": "example.howl",
     "line": 1,
     "string": "comment: This is an RDFS comment.\n",
     "parse-tree":
     ["STATEMENT_BLOCK",
      ["ARROWS", "", ""],
      ["LABEL", "comment"],
      ["DATATYPES"],
      ["COLON", "", ":", " "],
      "This is an RDFS comment."],
     "leading-whitespace": "",
     "trailing-whitespace": "\n",
     "arrows": "",
     "predicate-name": ["LABEL", "comment"],
     "use-default-datatypes": true,
     "datatype-names": ["PLAIN"],
     "content": "This is an RDFS comment.",
     "graph": "http://example.com/current-graph",
     "subject": "http://example.com/current-subject",
     "predicate": "http://www.w3.org/2000/01/rdf-schema#comment",
     "object": "This is an RDFS comment.",
     "datatypes": ["PLAIN"]}

This statement block:

    comment [@en]: This is an English comment.

is parsed into this JSON object:

    {"block-type": "STATEMENT_BLOCK",
     "source": "example.howl",
     "line": 1,
     "string": "comment [@en]: This is an English comment.\n",
     "parse-tree":
     ["STATEMENT_BLOCK",
      ["ARROWS", "", ""],
      ["LABEL", "comment"],
      ["DATATYPES", " [", ["LANGUAGE_TAG", "@", "en"], "]"],
      ["COLON", "", ":", " "],
      "This is an English comment."],
     "leading-whitespace": "",
     "trailing-whitespace": "\n",
     "arrows": "",
     "predicate-name": ["LABEL", "comment"],
     "use-default-datatypes": false,
     "datatype-names": [["LANGUAGE_TAG", "@", "en"]],
     "content": "This is an English comment.",
     "graph": "http://example.com/current-graph",
     "subject": "http://example.com/current-subject",
     "predicate": "http://www.w3.org/2000/01/rdf-schema#comment",
     "object": "This is an English comment.",
     "datatypes": ["@en"]}

This statement block:

    rdf:type [LINK]: owl:Class

is parsed into this JSON object:

    {"block-type": "STATEMENT_BLOCK",
     "source": "example.howl",
     "line": 1,
     "string": "rdf:type [LINK]: owl:Class\n",
     "parse-tree":
     ["STATEMENT_BLOCK",
      ["ARROWS", "", ""],
      ["PREFIXED_NAME", "rdf", ":", "type"]
      ["DATATYPES", " [", "LINK", "]"],
      ["COLON", "", ":", " "],
      ["PREFIXED_NAME", "owl", ":", "Class"]],
     "leading-whitespace": "",
     "trailing-whitespace": "\n",
     "arrows": "",
     "predicate-name": ["PREFIXED_NAME", "rdf", ":", "type"],
     "use-default-datatypes": false,
     "datatype-names": ["LINK"],
     "content": ["PREFIXED_NAME", "owl", ":", "Class"],
     "graph": "http://example.com/current-graph",
     "subject": "http://example.com/current-subject",
     "predicate": "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
     "object": "http://www.w3.org/2002/07/owl#Class",
     "datatypes": ["LINK"]}

We can assign default datatypes to labels. When that label is used as a predicate, and no datatype is specified, the default datatype will apply. We have assigned the special type "LINK" to the label "type":

    type: owl:Class

is parsed into this JSON object:

    {"block-type": "STATEMENT_BLOCK",
     "source": "example.howl",
     "line": 1,
     "string": "type: owl:Class\n",
     "parse-tree":
     ["STATEMENT_BLOCK",
      ["ARROWS", "", ""],
      ["LABEL", "type"]
      ["DATATYPES"],
      ["COLON", "", ":", " "],
      ["PREFIXED_NAME", "owl", ":", "Class"]],
     "leading-whitespace": "",
     "trailing-whitespace": "\n",
     "arrows": "",
     "predicate-name": ["LABEL", "type"],
     "use-default-datatypes": true,
     "datatype-names": ["LINK"],
     "content": ["PREFIXED_NAME", "owl", ":", "Class"],
     "graph": "http://example.com/current-graph",
     "subject": "http://example.com/current-subject",
     "predicate": "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
     "object": "http://www.w3.org/2002/07/owl#Class",
     "datatypes": ["LINK"]}

HOWL files can also contain OWL class expressions in the version of Manchester syntax familiar from the [Protégé](http://protege.stanford.edu) editor. The subject for the expression block will be whatever the current subject is, as specified in a previous subject block or graph block. The expression block consists of a predicate, a `:>> ` (colon, two arrows, and one or more spaces) separator, and an expression string. The predicate can be a prefixed name, IRI, or label.

This expression block:

    subclass of: 'has part' some foo

is parsed into this JSON object:

    {"block-type": "STATEMENT_BLOCK",
     "source": "example.howl",
     "line": 1,
     "string": "subclass of: 'has part' some foo\n",
     "parse-tree":
     ["STATEMENT_BLOCK",
      ["ARROWS", "", ""],
      ["LABEL", "subclass of"]
      ["DATATYPES"],
      ["COLON", "", ":", " "],
      ["MANCHESTER_EXPRESSION",
       ["CLASS_EXPRESSION",
        ["SOME",
         ["OBJECT_PROPERTY_EXPRESSION",
          ["LABEL", "'", "has part", "'"]],
         " ", "some", " ",
         ["CLASS_EXPRESSION",
          ["LABEL", "", "foo", ""]]]]]],
     "leading-whitespace": "",
     "trailing-whitespace": "\n",
     "arrows": "",
     "predicate-name": ["LABEL", "subclass of"],
     "use-default-datatypes": true,
     "datatype-names": ["LINK", ["LABEL", "Manchester"]],
     "content":
     ["MANCHESTER_EXPRESSION",
      ["CLASS_EXPRESSION",
       ["SOME",
        ["OBJECT_PROPERTY_EXPRESSION",
         ["LABEL", "'", "has part", "'"]],
        " ", "some", " ",
        ["CLASS_EXPRESSION",
         ["LABEL", "", "foo", ""]]]]],
     "graph": "http://example.com/current-graph",
     "subject": "http://example.com/current-subject",
     "predicate": "http://www.w3.org/2000/01/rdf-schema#subClassOf",
     "object":
     ["MANCHESTER_EXPRESSION",
      ["CLASS_EXPRESSION",
       ["SOME",
        ["OBJECT_PROPERTY_EXPRESSION",
         ["IRI", "http://purl.obolibrary.org/obo/BFO_0000050"]],
        ["CLASS_EXPRESSION",
         ["IRI", "http://example.com/foo"]]]]],
     "datatypes": ["LINK", "http://www.w3.org/TR/owl2-manchester-syntax/"]}


HOWL includes a convenient syntax for OWL annotations: one or more `>` "arrows" preceding a link block or literal block changes the subject to a previous statement.

This statement block:

    > comment: A comment on a comment.

is parsed into this JSON object:

    {"block-type": "STATEMENT_BLOCK",
     "source": "example.howl",
     "line": 1,
     "string": "> comment: A comment on a comment.\n",
     "parse-tree":
     ["STATEMENT_BLOCK",
      ["ARROWS", ">", " "],
      ["LABEL", "comment"],
      ["DATATYPES"],
      ["COLON", "", ":", " "],
      "A comment on a comment."],
     "leading-whitespace": "",
     "trailing-whitespace": "\n",
     "arrows": ">",
     "predicate-name": ["LABEL", "comment"],
     "use-default-datatypes": true,
     "datatype-names": ["PLAIN"],
     "content": "A comment on a comment.",
     "annotation-target":
     ["http://example.com/current-subject",
      "http://example.com/previous-predicate",
      "http://example.com/previous-object",
      "http://example.com/previous-datatype"],
     "graph": "http://example.com/current-graph",
     "subject": "_:b1",
     "predicate": "http://www.w3.org/2000/01/rdf-schema#comment",
     "object": "A comment on a comment.",
     "datatypes": ["PLAIN"]}
