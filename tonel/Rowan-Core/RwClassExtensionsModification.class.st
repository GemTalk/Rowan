"
No class-specific documentation for CypClassExtensionsModification, hierarchy is: 
Object
  CypElementsModification( elementsAdded elementsRemoved elementsModified)
    CypClassExtensionsModification

"
Class {
	#name : 'RwClassExtensionsModification',
	#superclass : 'RwElementsModification',
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwClassExtensionsModification >> acceptVisitor: aVisitor [

	^aVisitor visitClassExtensionsModification: self
]
