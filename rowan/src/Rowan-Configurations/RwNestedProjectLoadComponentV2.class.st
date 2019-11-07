Class {
	#name : 'RwNestedProjectLoadComponentV2',
	#superclass : 'RwAbstractProjectLoadComponentV2',
	#category : 'Rowan-Configurations'
}

{ #category : 'visiting' }
RwNestedProjectLoadComponentV2 >> acceptNestedVisitor: aVisitor [

	^aVisitor visitNestedProjectLoadConfiguration: self

]

{ #category : 'visiting' }
RwNestedProjectLoadComponentV2 >> acceptVisitor: aVisitor [

	^self error: 'nested configuration cannot be used as a top-level configuration. The receiver is nested inside of project load configurations'

]

{ #category : 'testing' }
RwNestedProjectLoadComponentV2 >> isIndependentlyLoadable [
	"nested configuration is not independently loadable ... they can only be loaded when referenced from another config"

	^ false
]
