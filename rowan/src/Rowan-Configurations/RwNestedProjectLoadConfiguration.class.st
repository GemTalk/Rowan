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

{ #category : 'testing' }
RwNestedProjectLoadConfiguration >> isIndependentlyLoadable [
	"nested configuration is not independently loadable ... they can only be loaded when referenced from another config"

	^ false
]
