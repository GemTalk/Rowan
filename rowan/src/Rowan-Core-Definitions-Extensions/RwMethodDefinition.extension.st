Extension { #name : 'RwMethodDefinition' }

{ #category : '*rowan-core-definitions-extensions' }
RwMethodDefinition >> compareAgainstBase: aDefinition [ 

	| modification |
	modification := RwMethodModification before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition);
		sourceModification: (self compareSourceAgainstBase: aDefinition).
	^modification

]

{ #category : '*rowan-core-definitions-extensions' }
RwMethodDefinition >> compareAgainstBaseForNewClassVersion: aDefinition [

	| modification |
	modification := RwMethodModificationForNewClassVersion
		before: aDefinition
		after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition);
		sourceModification:
				(self compareSourceAgainstBaseForNewClassVersion: aDefinition).
	^ modification
]

{ #category : '*rowan-core-definitions-extensions' }
RwMethodDefinition >> compareExtensionMethodsAgainstBase: aDefinition [

	| modification |
	modification := RwExtensionMethodModification before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition);
		sourceModification: (self compareSourceAgainstBase: aDefinition).
	^ modification
]

{ #category : '*rowan-core-definitions-extensions' }
RwMethodDefinition >> compareSourceAgainstBase: aDefinition [

	| modification |
	modification := RwSourceModification new.
	aDefinition source ~= self source
		ifTrue: [
			| before after |
			before := aDefinition source.
			after := self source.
			modification addElementModification: (RwPropertyModification
								key: 'source'
								oldValue: before
								newValue: after) ].
	^modification
]

{ #category : '*rowan-core-definitions-extensions' }
RwMethodDefinition >> compareSourceAgainstBaseForNewClassVersion: aDefinition [

	"unconditional modification for new class versions"

	| modification before after |
	modification := RwSourceModification new.
	before := aDefinition source.
	after := self source.
	modification
		addElementModification:
			(RwUnconditionalPropertyModification
				key: 'source'
				oldValue: before
				newValue: after).
	^ modification
]


