# Assay

This is an example of a [Markdown](http://commonmark.org) file containing explanatory text about an ontology, and defining the ontology using HOWL syntax.

    assay
    definition: A planned process with the objective to produce information about the material entity that is the evaluant, by physically examining it or its proxies.
    definition source: OBI branch derived
    example of usage: Assay the wavelength of light emitted by excited Neon atoms. Count of geese flying over a house.
    term editor: PlanAndPlannedProcess Branch

    alternative term: measuring
    alternative term: scientific observation

    equivalent to:>> achieves_planned_objective some 'assay objective'
    subclass of:>> 'planned process'
    subclass of:>> has_specified_input some
      ('material entity'
       and ('has role' some 'evaluant role'))
    subclass of:>> has_specified_output some
      ('information content entity'
       and ('is about' some
          ('material entity'
           and ('has role' some 'evaluant role'))))
    subclass of:>> realizes some 'evaluant role'

Only the indented lines are processed by HOWL, but humans might be interested in the rest of the text.
