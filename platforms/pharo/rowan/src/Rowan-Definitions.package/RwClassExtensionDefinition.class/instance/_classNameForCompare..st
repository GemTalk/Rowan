private
_classNameForCompare: aDefinition

	^ self key ifNil: 
		[ | cName |
		cName := aDefinition key.
		self name: cName.
		cName ]
