Class {
	#name : 'RwGsMethodDeletionSymbolDictPatchV2',
	#superclass : 'RwGsMethodPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'deleting' }
RwGsMethodDeletionSymbolDictPatchV2 >> deleteMethodNewClasses: createdClasses andExistingClasses: tempSymbols [

	self primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols.
	behavior
		ifNil: [ 
			"class is being deleted, nothing else to do"
			^ self ].

	self symbolDictionaryRegistry
		deleteMethod: methodDefinition selector
		for: behavior
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]

{ #category : 'deleting' }
RwGsMethodDeletionSymbolDictPatchV2 >> deleteMovedLoadedMethodNewClasses: createdClasses andExistingClasses: tempSymbols [

	self primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols.
	behavior
		ifNil: [ 
			"class is being deleted, nothing else to do"
			^ self ].

	self symbolDictionaryRegistry
		deleteMovedLoadedMethod: methodDefinition selector
		for: behavior
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]

{ #category : 'deleting' }
RwGsMethodDeletionSymbolDictPatchV2 >> deleteMovedMethodNewClasses: createdClasses andExistingClasses: tempSymbols [

	self primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols.
	behavior
		ifNil: [ 
			"class is being deleted, nothing else to do"
			^ self ].

	self symbolDictionaryRegistry
		deleteMovedMethod: methodDefinition selector
		for: behavior
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]

{ #category : 'deleting' }
RwGsMethodDeletionSymbolDictPatchV2 >> deleteNewVersionMethodNewClasses: createdClasses andExistingClasses: tempSymbols [

	"remove the method from deleted things"

	self symbolDictionaryRegistry
		_doDeleteCompiledMethodFromLoadedThings: self compiledMethod
		for: behavior
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]

{ #category : 'initializers' }
RwGsMethodDeletionSymbolDictPatchV2 >> isAnInitializer [

	"answer true if I represent a class initialization method"

	^ false

]

{ #category : 'initializers' }
RwGsMethodDeletionSymbolDictPatchV2 >> runInitializer [
	"noop"
]

{ #category : 'accessing' }
RwGsMethodDeletionSymbolDictPatchV2 >> symbolDictionary [
	"For deleting methods, we have to find the symbol dictionary where the loaded method is currently stored ... "

	^ Rowan image 
			loadedMethodAndSymbolDicitonaryForMethod: self compiledMethod 
			ifPresent: [ :symbolDict :loadedMethod | ^ symbolDict ]
			ifAbsent: [ ^ super symbolDictionary ]

]
