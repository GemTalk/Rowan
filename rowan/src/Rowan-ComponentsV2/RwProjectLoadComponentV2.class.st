Class {
	#name : 'RwProjectLoadComponentV2',
	#superclass : 'RwCommonProjectLoadComponentV2',
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'visiting' }
RwProjectLoadComponentV2 >> acceptVisitor: aVisitor [

	^aVisitor visitComponentLoadConfiguration: self
]
