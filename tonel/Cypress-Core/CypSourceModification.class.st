"
DELETE ME
"
Class {
	#name : 'CypSourceModification',
	#superclass : 'CypElementsModification',
	#category : 'Cypress-Core'
}

{ #category : 'visiting' }
CypSourceModification >> acceptVisitor: aVisitor [

	^aVisitor visitSourceModification: self
]
