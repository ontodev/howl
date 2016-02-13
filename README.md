# Humane OWL Format (HOWL)

HOWL is (an idea for) a new concrete syntax for RDF and OWL, designed for human editing and standard version control workflows.


## Example

    PREFIX ex: <http://example.com/>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

    DECLARE ex:ontology: Example Ontology
    type> Ontology

    DECLARE ex:foo: Foo
    type> Class
    comment: A comment on 'Foo'.@en
    > comment: An annotation on the comment^^xsd:string
    >> comment: An annotation on the annotation
    comment: Values can span multiple lines,
      and include blank lines...

      as long as each line is indented two spaces.
    subClassOf> 'has part' some 'Bar'
    # Comment: Class expressions use Manchester Syntax.

    DECLARE ex:bar: Bar
    type> Class

Overview:

- SPARQL-style PREFIXes
- DECLARE the IRI and rdfs:label in one line
- use a label, QName, or IRI for any named resource
- assert triples: subject (from previous DECLARE), predicate, and object
    - indicate that the object is a resource using `> `
    - indicate that the object is a literal using `: `
    - literals can end with datatype or language tag
- OWL annotations start with `>` and can be nested
- multi-line values use indentation
- class expressions use Manchester Syntax and labels
- lines starting with `#` are comments (not idempotent)


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

- do not contain newlines or tabs
- do not contain `:`
- do not contain `>`
- do not begin with `#`
- do not begin with spaces

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


## External Context

To avoid repetition within a project, shared context can be stored in separate files.

The prefixes can be defined in a separate file.

The ID-label mappings can be defined in a separate file, in which case the `DECLARE` statements can omit the ID.


### External Example

The same example as above, using an external set of prefixes and ID-label mappings.

    DECLARE: Example Ontology
    type> Ontology

    DECLARE: Foo
    type> Class
    comment: A comment on 'Foo'.@en
    > comment: An annotation on the comment^^xsd:string
    >> comment: An annotation on the annotation
    comment: Values can span multiple lines,
      and include blank lines...

      as long as each lines is indented two spaces.
    subClassOf> 'has part' some 'Bar'
    # Comment: Class expressions use Manchester Syntax.

    DECLARE: Bar
    type> Class


