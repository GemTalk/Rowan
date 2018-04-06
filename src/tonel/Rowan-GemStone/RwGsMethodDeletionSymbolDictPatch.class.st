"
No class-specific documentation for CypGsMethodDeletionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsMethodDeletionPatch( isMeta methodDefinition classDefinition packageDefinition)

"
Class {
	#name : 'RwGsMethodDeletionSymbolDictPatch',
	#superclass : 'RwGsMethodPatch',
	#category : 'Rowan-GemStone'
}

{ #category : 'deleting' }
RwGsMethodDeletionSymbolDictPatch >> deleteLoadedMethodNewClasses: createdClasses andExistingClasses: tempSymbols [

	self primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols.
	behavior
		ifNil: [ 
			"class cannot be found, so the method is already gone"
			^ self ].

	self packageSymbolDictionary
		_doDeleteCompiledMethodFromLoadedThings: self compiledMethod
		for: behavior
]

{ #category : 'deleting' }
RwGsMethodDeletionSymbolDictPatch >> deleteMethodNewClasses: createdClasses andExistingClasses: tempSymbols [

	self primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols.
	behavior
		ifNil: [ 
			"class is being deleted, nothing else to do"
			^ self ].

	self packageSymbolDictionary
		deleteMethod: methodDefinition selector
		for: behavior
]

{ #category : 'deleting' }
RwGsMethodDeletionSymbolDictPatch >> deleteNewVersionMethodNewClasses: createdClasses andExistingClasses: tempSymbols [

	"remove the method from deleted things"

	self packageSymbolDictionary
		_doDeleteCompiledMethodFromLoadedThings: self compiledMethod
		for: behavior
]

{ #category : 'initializers' }
RwGsMethodDeletionSymbolDictPatch >> runInitializer [
	"noop"
]
