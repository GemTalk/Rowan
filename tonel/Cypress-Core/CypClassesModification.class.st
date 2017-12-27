"
A modification showing the difference between the classes or classExtensions of a package. The elements are classes. Key is class name.
"
Class {
	#name : 'CypClassesModification',
	#superclass : 'CypElementsModification',
	#category : 'Cypress-Core'
}

{ #category : 'visiting' }
CypClassesModification >> acceptVisitor: aVisitor [

	^aVisitor visitClassesModification: self
]
