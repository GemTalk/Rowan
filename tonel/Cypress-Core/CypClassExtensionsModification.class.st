"
No class-specific documentation for CypClassExtensionsModification, hierarchy is: 
Object
  CypElementsModification( elementsAdded elementsRemoved elementsModified)
    CypClassExtensionsModification

"
Class {
	#name : 'CypClassExtensionsModification',
	#superclass : 'CypElementsModification',
	#category : 'Cypress-Core'
}

{ #category : 'visiting' }
CypClassExtensionsModification >> acceptVisitor: aVisitor [

	^aVisitor visitClassExtensionsModification: self
]
