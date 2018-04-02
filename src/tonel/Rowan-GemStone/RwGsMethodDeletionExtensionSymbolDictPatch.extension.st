Extension { #name : 'RwGsMethodDeletionExtensionSymbolDictPatch' }

{ #category : '*rowan-gemstone' }
RwGsMethodDeletionExtensionSymbolDictPatch >> primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols [

	| className class |
	className := extendedClassName asSymbol.
	class := createdClasses
		at: className
		ifAbsent: [ 
			tempSymbols
				at: className
				ifAbsent: [ 
					self classDefinition key
						ifNil: [ 
							"class is being deleted ... we're done"
							^ self ].
					self error: 'Cannot find class to delete method from.' ] ].
	behavior := isMeta
		ifTrue: [ class class ]
		ifFalse: [ class ]
]
