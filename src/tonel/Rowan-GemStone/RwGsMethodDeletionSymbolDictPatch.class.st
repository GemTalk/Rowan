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

	classDefinition key
		ifNil: [ 
			"class is being deleted ... we're done"
			^ self ].

	self
		primeCompiledMethodNewClasses: createdClasses
		andExistingClasses: tempSymbols.

	self packageSymbolDictionary
		_doDeleteCompiledMethodFromLoadedThings: self compiledMethod
		for: behavior
]

{ #category : 'deleting' }
RwGsMethodDeletionSymbolDictPatch >> deleteMethodNewClasses: createdClasses andExistingClasses: tempSymbols [

	classDefinition key
		ifNil: [ 
			"class is being deleted ... we're done"
			^ self ].
	self
		primeCompiledMethodNewClasses: createdClasses
		andExistingClasses: tempSymbols.

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

{ #category : 'deleting' }
RwGsMethodDeletionSymbolDictPatch >> primeCompiledMethodNewClasses: createdClasses andExistingClasses: tempSymbols [

	| className class |
	classDefinition key
		ifNil: [ 
			"class is being deleted ... we're done"
			^ self ].
	className := classDefinition key asSymbol.
	class := createdClasses
		at: className
		ifAbsent: [ 
			tempSymbols
				at: className
				ifAbsent: [ self error: 'Cannot find class to delete method from.' ] ].
	behavior := isMeta
		ifTrue: [ class class ]
		ifFalse: [ class ]
]

{ #category : 'initializers' }
RwGsMethodDeletionSymbolDictPatch >> runInitializer [
	"noop"
]
