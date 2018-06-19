Class {
	#name : 'RwGsMethodDeletionExtensionSymbolDictPatch',
	#superclass : 'RwGsMethodDeletionSymbolDictPatch',
	#instVars : [
		'extendedClassName'
	],
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'accessing' }
RwGsMethodDeletionExtensionSymbolDictPatch >> className [

	^ self extendedClassName
]

{ #category : 'accessing' }
RwGsMethodDeletionExtensionSymbolDictPatch >> extendedClassName [

   ^extendedClassName

]

{ #category : 'accessing' }
RwGsMethodDeletionExtensionSymbolDictPatch >> extendedClassName: anObject [

	extendedClassName := anObject
]

{ #category : 'private' }
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

{ #category : 'accessing' }
RwGsMethodDeletionExtensionSymbolDictPatch >> symbolDictionary [
        "For deleting methods, we have to find the symbol dictionary where the loaded method is currently stored ... "

        Rowan image 
                loadedMethodAndSymbolDicitonaryForMethod: self compiledMethod 
                ifPresent: [ :symbolDict :loadedMethod | ^ symbolDict ]
                ifAbsent: [ self error: 'internal error - no symbol dictionary found for the method ', methodDefinition selector printString ]

]
