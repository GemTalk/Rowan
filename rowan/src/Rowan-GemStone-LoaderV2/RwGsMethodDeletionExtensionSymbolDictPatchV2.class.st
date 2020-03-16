Class {
	#name : 'RwGsMethodDeletionExtensionSymbolDictPatchV2',
	#superclass : 'RwGsMethodDeletionSymbolDictPatchV2',
	#instVars : [
		'extendedClassName'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'accessing' }
RwGsMethodDeletionExtensionSymbolDictPatchV2 >> className [

	^ self extendedClassName
]

{ #category : 'accessing' }
RwGsMethodDeletionExtensionSymbolDictPatchV2 >> extendedClassName [

   ^extendedClassName

]

{ #category : 'accessing' }
RwGsMethodDeletionExtensionSymbolDictPatchV2 >> extendedClassName: anObject [

	extendedClassName := anObject
]

{ #category : 'private' }
RwGsMethodDeletionExtensionSymbolDictPatchV2 >> primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols [

	| className class |
	className := extendedClassName asSymbol.
	class := createdClasses
		at: className
		ifAbsent: [ 
			tempSymbols
				at: className
				ifAbsent: [ 
					"class is being deleted ... we're done"
					^ self] ].
	behavior := isMeta
		ifTrue: [ class class ]
		ifFalse: [ class ]
]
