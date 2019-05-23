*rowan-core-definitions-extensions
compareSourceAgainstBase: aDefinition

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