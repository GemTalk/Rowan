Class {
	#name : 'RwGsMethodDeletionExtensionSymbolDictPatch',
	#superclass : 'RwGsMethodDeletionSymbolDictPatch',
	#instVars : [
		'extendedClassName'
	],
	#category : 'Rowan-GemStone'
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
