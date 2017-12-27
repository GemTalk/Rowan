"
A modification showing the difference between the instance or class methods defined by a class or class extension. The elements are method definitions. Key is selector (as string).
"
Class {
	#name : 'CypMethodsModification',
	#superclass : 'CypElementsModification',
	#category : 'Cypress-Core'
}

{ #category : 'visiting' }
CypMethodsModification >> acceptVisitor: aVisitor [

	^aVisitor visitMethodsModification: self
]
