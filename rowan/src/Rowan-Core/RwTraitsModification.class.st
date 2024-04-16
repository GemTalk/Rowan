Class {
	#name : 'RwTraitsModification',
	#superclass : 'RwElementsModification',
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwTraitsModification >> acceptVisitor: aVisitor [

	^aVisitor visitTraitsModification: self
]
