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

	self primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols.
	behavior
		ifNil: [ 
			"class cannot be found, so the method is already gone"
			^ self ].

	self
		updateMethodDictionary;
		updateProtocol;
		deleteLoadedMethod
]

{ #category : 'initializers' }
RwGsMethodDeletionPatch >> isAnInitializer [

	"answer true if I represent a class initialization method"

	^ false

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
