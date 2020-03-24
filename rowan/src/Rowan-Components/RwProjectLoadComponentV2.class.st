Class {
	#name : 'RwProjectLoadComponentV2',
	#superclass : 'RwAbstractProjectLoadComponentV2',
	#category : 'Rowan-Components'
}

{ #category : 'visiting' }
RwProjectLoadComponentV2 >> acceptVisitor: aVisitor [

	^aVisitor visitProjectLoadComponent: self
]
