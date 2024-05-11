Class {
	#name : 'RwGsTraitMethodDeletionSymbolDictPatchV2',
	#superclass : 'RwGsMethodDeletionSymbolDictPatchV2',
	#instVars : [
		'traitDefinition',
		'traitInstance'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'deleting' }
RwGsTraitMethodDeletionSymbolDictPatchV2 >> deleteMethodNewClassesSymbolList: createdClasses andExistingClasses: tempSymbols [
	self
		primeBehaviorNewTraitsSymbolList: createdClasses
		andExistingTraits: tempSymbols.
	traitInstance
		ifNil: [ 
			"trait is being deleted, nothing else to do"
			^ self ].

	self symbolDictionaryRegistry
		deleteMethod: methodDefinition selector
		for: behavior
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]

{ #category : 'deleting' }
RwGsTraitMethodDeletionSymbolDictPatchV2 >> deleteNewVersionMethodNewClassesSymbolList: createdClasses andExistingClasses: tempSymbols [
	"remove the method from deleted things"

	"behavior is set, by an earlier call to deleteMethodNewClasses:andExistingClassSymbolList: "

	self symbolDictionaryRegistry
		_doDeleteCompiledMethodFromLoadedThings: self compiledMethod
		for: behavior
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]

{ #category : 'private' }
RwGsTraitMethodDeletionSymbolDictPatchV2 >> primeBehaviorNewTraitsSymbolList: createdTraits andExistingTraits: tempSymbols [
	| traitName trait symDictName |
	traitDefinition key
		ifNil: [ 
			"trait is being deleted ... we're done"
			^ self ].
	traitName := traitDefinition key asSymbol.
	symDictName := self symbolDictionaryName.
	trait := (RwGsPatchSet_V2 lookupSymbolDictName: symDictName in: createdTraits)
		at: traitName
		ifAbsent: [ 
			tempSymbols
				at: traitName
				ifAbsent: [ 
					"cannot find trait ... caller can decide whether or not that is a problem -- traitInstance will be nil"
					^ self ] ].
	traitInstance :=  trait
]

{ #category : 'accessing' }
RwGsTraitMethodDeletionSymbolDictPatchV2 >> traitDefinition [
	^traitDefinition
]

{ #category : 'accessing' }
RwGsTraitMethodDeletionSymbolDictPatchV2 >> traitDefinition: object [
	traitDefinition := object
]
