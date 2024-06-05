Extension { #name : 'RwTraitDefinition' }

{ #category : '*rowan-gemstone-definitions' }
RwTraitDefinition >> _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue [
	(#(#'_gsFileOffset' #'_gsFileName' #'commentStamp')
		includesIdentical: propertyKey)
		ifTrue: [ 
			"_gsFileOffset' '_gsFileName' 'commentStamp' are informational properties so any differences can be ignored "
			^ true ].
	^ super
		_compareProperty: propertyKey
		propertyVaue: propertyValue
		againstBaseValue: baseValue
]

{ #category : '*rowan-gemstone-definitions' }
RwTraitDefinition >> gs_symbolDictionary [

	^ self properties
		at: #'gs_SymbolDictionary'
		ifAbsent: []

]

{ #category : '*rowan-gemstone-definitions' }
RwTraitDefinition >> gs_symbolDictionary: aSymbolDictionaryName [

	self properties
		at: #'gs_SymbolDictionary'
		put: aSymbolDictionaryName asString

]
