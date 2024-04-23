Extension { #name : 'RwTraitDefinition' }

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
