private
compareEqualTo: aRwSemanticVersionNumber
    aRwSemanticVersionNumber species = self species
        ifFalse: [ ^ false ].
    (self compareEqualTo: self normalVersion other: aRwSemanticVersionNumber normalVersion)
        ifFalse: [ ^ false ].
    (self compareEqualTo: self preReleaseVersion other: aRwSemanticVersionNumber preReleaseVersion)
        ifFalse: [ ^ false ].
    ^ true