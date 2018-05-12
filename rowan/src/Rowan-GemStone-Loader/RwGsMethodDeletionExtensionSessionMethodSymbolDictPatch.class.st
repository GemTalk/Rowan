Class {
	#name : 'RwGsMethodDeletionExtensionSessionMethodSymbolDictPatch',
	#superclass : 'RwGsMethodDeletionExtensionSymbolDictPatch',
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'accessing' }
RwGsMethodDeletionExtensionSessionMethodSymbolDictPatch >> compiledMethod [

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

{ #category : 'deleting' }
RwGsMethodDeletionExtensionSessionMethodSymbolDictPatch >> deleteMethodNewClasses: createdClasses andExistingClasses: tempSymbols [

	self primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols.
	behavior
		ifNil: [ 
			"class cannot be found, so the method is already gone"
			^ self ].

	self symbolDictionaryRegistry
		deleteMethod: methodDefinition selector
		for: behavior
]

{ #category : 'deleting' }
RwGsMethodDeletionExtensionSessionMethodSymbolDictPatch >> deleteNewVersionMethodNewClasses: createdClasses andExistingClasses: tempSymbols [

	"noop for class extension methods"

	
]
