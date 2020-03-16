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

{ #category : 'deleting' }
RwGsMethodDeletionExtensionSessionMethodSymbolDictPatchV2 >> deleteMethodNewClasses: createdClasses andExistingClasses: tempSymbols [

	self primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols.
	behavior
		ifNil: [ 
			"class cannot be found, so the method is already gone"
			^ self ].

	self symbolDictionaryRegistry
		deleteMethod: methodDefinition selector
		for: behavior
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]

{ #category : 'deleting' }
RwGsMethodDeletionExtensionSessionMethodSymbolDictPatchV2 >> deleteMovedLoadedMethodNewClasses: createdClasses andExistingClasses: tempSymbols [

	self primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols.
	behavior
		ifNil: [ 
			"class cannot be found, so the method is already gone"
			^ self ].

	self symbolDictionaryRegistry
		deleteMovedLoadedMethod: methodDefinition selector
		for: behavior
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]

{ #category : 'deleting' }
RwGsMethodDeletionExtensionSessionMethodSymbolDictPatchV2 >> deleteMovedMethodNewClasses: createdClasses andExistingClasses: tempSymbols [

	self primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols.
	behavior
		ifNil: [ 
			"class cannot be found, so the method is already gone"
			^ self ].

	self symbolDictionaryRegistry
		deleteMovedMethod: methodDefinition selector
		for: behavior
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]

{ #category : 'deleting' }
RwGsMethodDeletionExtensionSessionMethodSymbolDictPatchV2 >> deleteNewVersionMethodNewClasses: createdClasses andExistingClasses: tempSymbols [
	"noop for class extension methods"
]

{ #category : 'accessing' }
RwGsMethodDeletionExtensionSessionMethodSymbolDictPatchV2 >> symbolDictionary [

	^self symbolDictionaryFor: self packageName
]
