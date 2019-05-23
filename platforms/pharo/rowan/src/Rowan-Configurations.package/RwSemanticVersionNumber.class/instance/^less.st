comparing
< aRwSemanticVersionNumber
    aRwSemanticVersionNumber species = self species
        ifFalse: [ ^ false ].
    ^ self compareLessThan: aRwSemanticVersionNumber
