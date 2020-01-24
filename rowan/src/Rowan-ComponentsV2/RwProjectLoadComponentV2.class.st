Class {
	#name : 'RwProjectLoadComponentV2',
	#superclass : 'RwAbstractProjectLoadComponentV2',
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'visiting' }
RwProjectLoadComponentV2 >> acceptVisitor: aVisitor [

	^aVisitor visitComponentLoadConfiguration: self
]
