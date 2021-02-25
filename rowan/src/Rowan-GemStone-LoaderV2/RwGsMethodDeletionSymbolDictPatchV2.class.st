Class {
	#name : 'RwGsMethodDeletionSymbolDictPatchV2',
	#superclass : 'RwGsMethodPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'deleting' }
RwGsMethodDeletionSymbolDictPatchV2 >> deleteMethodNewClassesSymbolList: createdClasses andExistingClasses: tempSymbols [
	self
		primeBehaviorNewClassesSymbolList: createdClasses
		andExistingClasses: tempSymbols.
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
RwGsMethodDeletionSymbolDictPatchV2 >> deleteNewVersionMethodNewClassesSymbolList: createdClasses andExistingClasses: tempSymbols [
	"remove the method from deleted things"

	"behavior is set, by an earlier call to deleteMethodNewClasses:andExistingClassSymbolList: "

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
