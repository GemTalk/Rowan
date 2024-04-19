Extension { #name : 'RwTraitMethodDefinition' }

{ #category : '*rowan-core-definitions-extensions' }
RwTraitMethodDefinition >> compareAgainstBase: aDefinition [ 

	| modification |
	modification := RwTraitMethodModification before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition);
		sourceModification: (self compareSourceAgainstBase: aDefinition).
	^modification
]
