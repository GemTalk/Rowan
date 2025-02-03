Extension { #name : 'RwClassExtensionDefinition' }

{ #category : '*rowan-gemstone-definitions' }
RwClassExtensionDefinition >> _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue [
	(#(#'_gsFileOffset' #'_gsFileName' ) includesIdentical: propertyKey)
		ifTrue: [ 
			"_gsFileOffset' '_gsFileName' are informational properties so any differences can be ignored "
			^ true ].
	^ super
		_compareProperty: propertyKey
		propertyVaue: propertyValue
		againstBaseValue: baseValue
]
