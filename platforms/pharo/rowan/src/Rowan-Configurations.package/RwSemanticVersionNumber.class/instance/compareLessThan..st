private
compareLessThan: aRwSemanticVersionNumber
    | myComponents otherComponents defaultResult |
    aRwSemanticVersionNumber species = self species
        ifFalse: [ ^ false ].
    myComponents := self normalVersion.
    otherComponents := aRwSemanticVersionNumber normalVersion.
    defaultResult := true.
    (self compareEqualTo: myComponents other: otherComponents)
        ifTrue: [ defaultResult := false ]
        ifFalse: [ 
            (self compareLessThan: myComponents other: otherComponents version: #'normal')
                ifFalse: [ ^ false ] ].
    myComponents := self preReleaseVersion.
    otherComponents := aRwSemanticVersionNumber preReleaseVersion.
    (self compareEqualTo: myComponents other: otherComponents)
        ifTrue: [ 
            myComponents size > 0
                ifTrue: [ defaultResult := false ] ]
        ifFalse: [ ^ self compareLessThan: myComponents other: otherComponents version: #'preRelease' ].
    ^ defaultResult