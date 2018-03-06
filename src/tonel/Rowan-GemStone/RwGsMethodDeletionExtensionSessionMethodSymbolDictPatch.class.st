Class {
	#name : 'RwGsMethodDeletionExtensionSessionMethodSymbolDictPatch',
	#superclass : 'RwGsMethodDeletionSymbolDictPatch',
	#category : 'Rowan-GemStone'
}

{ #category : 'accessing' }
RwGsMethodDeletionExtensionSessionMethodSymbolDictPatch >> compiledMethod [

	^ compiledMethod
		ifNil: [ 
			| dictsArray mDict |
			dictsArray := self packageSymbolDictionary homeSessionMethods
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
RwGsMethodDeletionExtensionSessionMethodSymbolDictPatch >> deleteLoadedMethodNewClasses: createdClasses andExistingClasses: tempSymbols [

	| className class |
	classDefinition key
		ifNil: [ 
			"class is being deleted ... we're cool"
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
		ifFalse: [ class ].
	self packageSymbolDictionary
		_doDeleteCompiledMethodFromLoadedThings: self compiledMethod
		for: behavior
]

{ #category : 'deleting' }
RwGsMethodDeletionExtensionSessionMethodSymbolDictPatch >> deleteMethodNewClasses: createdClasses andExistingClasses: tempSymbols [

	| className class |
	classDefinition key
		ifNil: [ 
			"class is being deleted ... we're cool"
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
		ifFalse: [ class ].
	self halt.
	self packageSymbolDictionary
		deleteMethod: methodDefinition selector
		for: behavior
]
