Class {
	#name : 'RwNestedProjectLoadConfiguration',
	#superclass : 'RwAbstractProjectLoadConfiguration',
	#category : 'Rowan-Configurations'
}

{ #category : 'visiting' }
RwNestedProjectLoadConfiguration >> acceptNestedVisitor: aVisitor [

	^aVisitor visitNestedProjectLoadConfiguration: self

]

{ #category : 'visiting' }
RwNestedProjectLoadConfiguration >> acceptVisitor: aVisitor [

	^self error: 'nested configuration cannot be used as a top-level configuration. The receiver is nested inside of project load configurations'

]
