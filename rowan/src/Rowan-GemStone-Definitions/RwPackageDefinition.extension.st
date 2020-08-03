Extension { #name : 'RwPackageDefinition' }

{ #category : '*rowan-gemstone-definitions' }
RwPackageDefinition >> _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue [
false ifTrue: [
"gs_SymbolDictionary is now explicitly set before load, so no longer special case for nil"
 	propertyKey = 'gs_SymbolDictionary'
		ifTrue: [ 
			propertyValue = baseValue
				ifTrue: [ ^ true ]
				ifFalse: [ 
					"if one or the other is nil, then count it as equal"
					^ propertyValue == nil or: [ baseValue == nil ] ] ].
].
	^ super
		_compareProperty: propertyKey
		propertyVaue: propertyValue
		againstBaseValue: baseValue
]

{ #category : '*rowan-gemstone-definitions' }
RwPackageDefinition >> gs_symbolDictionary [

	^ self properties
		at: 'gs_SymbolDictionary'
		ifAbsent: []

]

{ #category : '*rowan-gemstone-definitions' }
RwPackageDefinition >> gs_symbolDictionary: aSymbolDictionaryName [

	self properties
		at: 'gs_SymbolDictionary'
		put: aSymbolDictionaryName asString

]
