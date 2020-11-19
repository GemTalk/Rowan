Class {
	#name : 'RwSubcomponent',
	#superclass : 'RwComponent',
	#instVars : [ 'condition' ],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'visiting' }
RwSubcomponent >> acceptNestedVisitor: aVisitor [
	^ aVisitor visitSimpleProjectLoadComponent: self
]

{ #category : 'visiting' }
RwSubcomponent >> acceptVisitor: aVisitor [
	^ self
		error:
			'nested component cannot be used as a top-level configuration. The receiver is nested inside of top-level components'
]

{ #category : 'accessing' }
RwAbstractComponent >> condition [
	^condition
]

{ #category : 'accessing' }
RwAbstractComponent >> condition: aString [
	aString isString
		ifFalse: [ self error: 'The condition is constrained to be a string' ].
	condition := aString
]

{ #category : 'initialization' }
RwAbstractComponent >> initialize [
	super initialize.
	condition := 'common'.
]
