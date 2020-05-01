Class {
	#name : 'RwGsMethodDeletionExtensionSessionMethodSymbolDictPatchV2',
	#superclass : 'RwGsMethodDeletionExtensionSymbolDictPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'accessing' }
RwGsMethodDeletionExtensionSessionMethodSymbolDictPatchV2 >> compiledMethod [

	^ compiledMethod
		ifNil: [ 
			| dictsArray mDict |
			dictsArray := self symbolDictionaryRegistry homeSessionMethods
				methodAndCategoryDictionaryFor: behavior.
			mDict := dictsArray at: 1.
			selector := methodDefinition selector.
			compiledMethod := mDict
				at: selector
				ifAbsent: [ 
					self
						error:
							'internal error - compiled session method not present in method dictionary' ] ]
]

{ #category : 'accessing' }
RwGsMethodDeletionExtensionSessionMethodSymbolDictPatchV2 >> symbolDictionary [

	^self symbolDictionaryFor: self packageName
]
