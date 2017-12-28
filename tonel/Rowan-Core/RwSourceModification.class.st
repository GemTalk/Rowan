"
DELETE ME
"
Class {
	#name : 'RwSourceModification',
	#superclass : 'RwElementsModification',
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwSourceModification >> acceptVisitor: aVisitor [

	^aVisitor visitSourceModification: self
]
