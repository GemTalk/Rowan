Class {
	#name : 'RwGsPatchSetRowanBootstrap',
	#superclass : 'RwGsPatchSetBasic',
	#category : 'Rowan-GemStone'
}

{ #category : 'private - applying' }
RwGsPatchSetRowanBootstrap >> addAddedClassesToTempSymbols [
	"During bootstrap we expect all added classes to have an existing association"

	addedClasses do: [:patch | | assoc symDict |
		symDict := patch symbolDictionary.
		assoc := symDict associationAt: patch className asSymbol otherwise: nil.
		assoc ifNil: [ self error: 'expected class ', patch className printString ,' to exist during bootstrap step' ].
		tempSymbols addAssociation: assoc ]

]
