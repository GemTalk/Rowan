Extension { #name : 'Class' }

{ #category : '*rowan-gemstone' }
Class >> rwByteSubclass: aString classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aSymbolDictionary category: aCategoryName options: optionsArray [

	^ Rowan projectTools browser
		addOrUpdateClassDefinition: aString
		type: 'bytes'
		superclass: self name asString
		instVarNames: #()
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: aSymbolDictionary
		category: aCategoryName
		packageName: aCategoryName
		options: optionsArray
]

{ #category : '*rowan-gemstone' }
Class >> rwByteSubclass: aString classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aSymbolDictionary category: aCategoryName packageName: aPackageName options: optionsArray [

	^ Rowan projectTools browser
		addOrUpdateClassDefinition: aString
		type: 'bytes'
		superclass: self name asString
		instVarNames: #()
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: aSymbolDictionary
		category: aCategoryName
		packageName: aPackageName
		options: optionsArray
]

{ #category : '*rowan-gemstone' }
Class >> rwIndexableSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aSymbolDictionary category: aCategoryName options: optionsArray [

	^ Rowan projectTools browser
		addOrUpdateClassDefinition: aString
		type: 'variable'
		superclass: self name asString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: aSymbolDictionary
		category: aCategoryName
		packageName: aCategoryName
		options: optionsArray
]

{ #category : '*rowan-gemstone' }
Class >> rwIndexableSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aSymbolDictionary category: aCategoryName packageName: aPackageName options: optionsArray [

	^ Rowan projectTools browser
		addOrUpdateClassDefinition: aString
		type: 'variable'
		superclass: self name asString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: aSymbolDictionary
		category: aCategoryName
		packageName: aPackageName
		options: optionsArray
]

{ #category : '*rowan-gemstone' }
Class >> rwSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aSymbolDictionary category: aCategoryName options: optionsArray [

	^ Rowan projectTools browser
		addOrUpdateClassDefinition: aString
		type: 'normal'
		superclass: self name asString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: aSymbolDictionary
		category: aCategoryName
		packageName: aCategoryName
		options: optionsArray
]

{ #category : '*rowan-gemstone' }
Class >> rwSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aSymbolDictionary category: aCategoryName packageName: aPackageName options: optionsArray [

	^ Rowan projectTools browser
		addOrUpdateClassDefinition: aString
		type: 'normal'
		superclass: self name asString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: aSymbolDictionary
		category: aCategoryName
		packageName: aPackageName
		options: optionsArray
]
