Class {
	#name : 'RwSubcomponent',
	#superclass : 'RwComponent',
	#instVars : [
		'condition'
	],
	#category : 'Rowan-Components'
}

{ #category : 'visiting' }
RwSubcomponent >> acceptNestedVisitor: aVisitor [
	^ aVisitor visitComponent: self
]

{ #category : 'visiting' }
RwSubcomponent >> acceptVisitor: aVisitor [
	^ self
		error:
			'nested component cannot be used as a top-level configuration. The receiver is nested inside of top-level components'
]

{ #category : 'accessing' }
RwSubcomponent >> condition [
	^condition
]

{ #category : 'accessing' }
RwSubcomponent >> condition: aString [
	aString isString
		ifFalse: [ self error: 'The condition is constrained to be a string' ].
	condition := aString
]

{ #category : 'accessing' }
RwSubcomponent >> conditionalPropertyMatchers [
	^ Dictionary new
		at: {(self _platformPatternMatcherFor: self condition)} put: {};
		yourself
]

{ #category : 'ston' }
RwSubcomponent >> excludedInstVars [
	"restore full #instVarNamesInOrderForSton - no exclusions (see super implementation)"

	^ #()
]

{ #category : 'initialization' }
RwSubcomponent >> initialize [
	super initialize.
	condition := 'common'.
]
