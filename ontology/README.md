# Building an Ontology with HOWL

HOWL can be used to build a complete ontology. We can divide the content into different files, each suited to a specific purpose:

- [prefixes.howl](ontology/prefixes.howl): shared prefixes
- [labels.howl](ontology/labels.howl): shared labels
- [terms.tsv](ontology/terms.tsv): a table of all the terms, their labels, and their types
- [core.md](ontology/core.md): an essay about the ontology, defining the terms and explaining the wider context, side-by-side

Breaking the ontology into pieces gives us several advantages.

1. The [terms.tsv](ontology/terms.tsv) table keeps track of all our terms in one place. We can use tables whenever our terms have a strong, repeated pattern.
2. The [core.md](ontology/core.md) essay lets us explain the *why* of our ontology terms in a narrative form.
3. The [prefixes.howl](ontology/prefixes.howl) and [labels.howl](ontology/labels.howl) keep the technical details out-of-the-way so we can focus on the content.

A [script](Makefile#L56) assembles these pieces, and converts the result to an `.owl` file that we can open with [Protégé](http://protege.stanford.edu) or any of our existing tools.

