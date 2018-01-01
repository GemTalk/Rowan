"
No class-specific documentation for CypGsMethodDeletionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsMethodDeletionPatch( isMeta methodDefinition classDefinition packageDefinition)

"
Class {
	#name : 'RwGsMethodDeletionPatch',
	#superclass : 'RwGsMethodPatch',
	#category : 'Rowan-GemStone'
}

{ #category : 'private-deleting' }
RwGsMethodDeletionPatch >> deleteLoadedMethod [
	| loadedMethod loadedClassOrExtension |
	loadedMethod := RwGsLoadedMethod existingForMethod: compiledMethod
				ifAbsent: [
					self error: 'Internal error -- no existing LoadedMethod found for deleted method.'].
	loadedMethod unregisterYourself.
	loadedClassOrExtension := self loadedClassOrExtensionForMethod.
	loadedClassOrExtension removeLoadedMethod: loadedMethod.

]

{ #category : 'deleting' }
RwGsMethodDeletionPatch >> deleteMethodNewClasses: createdClasses andExistingClasses: tempSymbols [

	| className class |
	classDefinition key ifNil: [ "class is being deleted ... we're cool" ^self ].
	className := classDefinition key asSymbol.
	class := createdClasses at: className
				ifAbsent: 
					[tempSymbols at: className
						ifAbsent: [self error: 'Cannot find class to delete method from.']].
	behavior := isMeta ifTrue: [class class] ifFalse: [class].
	self
		updateMethodDictionary;
		updateProtocol;
		deleteLoadedMethod
]

{ #category : 'initializers' }
RwGsMethodDeletionPatch >> runInitializer [
	"noop"
]

{ #category : 'private-deleting' }
RwGsMethodDeletionPatch >> updateMethodDictionary [

	| methodDictionary |
	methodDictionary := behavior persistentMethodDictForEnv: 0.
	selector := methodDefinition selector.
	compiledMethod := methodDictionary 
		removeKey: selector 
		ifAbsent: [ self error: 'internal error - deleted method not present in method dictionary' ]
]

{ #category : 'private-deleting' }
RwGsMethodDeletionPatch >> updateProtocol [

	| cDict |
	cDict := behavior _baseCategorys: 0 .
	cDict keysAndValuesDo: [:aKey :setOfSelectors |
		(setOfSelectors remove: selector otherwise: nil ) ifNotNil:[ ^ self ] ].
	self error: 'internal error - deleted selector not preesnt in method protocol'
]
