Extension { #name : 'RwPackageDefinition' }

{ #category : '*rowan-gemstone-definitions' }
RwPackageDefinition >> gs_symbolDictionary [

	^ self properties
		at: #'gs_SymbolDictionary'
		ifAbsent: []

]

{ #category : '*rowan-gemstone-definitions' }
RwPackageDefinition >> gs_symbolDictionary: aSymbolDictionaryName [

	self properties
		at: #'gs_SymbolDictionary'
		put: aSymbolDictionaryName asString

]
