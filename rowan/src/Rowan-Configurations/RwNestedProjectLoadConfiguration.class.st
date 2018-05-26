Class {
	#name : 'RwNestedProjectLoadConfiguration',
	#superclass : 'RwAbstractProjectLoadConfiguration',
	#category : 'Rowan-Configurations'
}

{ #category : 'visiting' }
RwNestedProjectLoadConfiguration >> acceptVisitor: aVisitor [

	^aVisitor visitNestedProjectLoadConfiguration: self

]
