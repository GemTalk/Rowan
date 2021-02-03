Class {
	#name : 'RwPackageGroup',
	#superclass : 'RwAbstractComponent',
	#instVars : [
		'condition'
	],
	#category : 'Rowan-Components'
}

{ #category : 'dispatching' }
RwPackageGroup >> _addToResolvedProjectComponents: aRwResolvedProjectComponentsV2 [
	"dispatch to _addActiveComponent: or _addPackageGroup: as appropriate"

	aRwResolvedProjectComponentsV2 _addPackageGroup: self
]

{ #category : 'visiting' }
RwPackageGroup >> acceptNestedVisitor: aVisitor [
	^ aVisitor visitPackageGroupComponent: self
]

{ #category : 'accessing' }
RwPackageGroup >> condition [
	^condition
]

{ #category : 'accessing' }
RwPackageGroup >> condition: aString [
	aString isString
		ifFalse: [ self error: 'The condition is constrained to be a string' ].
	condition := aString
]

{ #category : 'accessing' }
RwPackageGroup >> conditionalPropertyMatchers [
	^ Dictionary new
		at: {(self _platformPatternMatcherFor: self condition)} put: {};
		yourself
]

{ #category : 'initialization' }
RwPackageGroup >> initialize [
	super initialize.
	condition := 'common'.
]

{ #category : 'ston' }
RwPackageGroup >> instVarNamesInOrderForSton [
	^ #(#'name' #'condition' #'componentNames' #'packageNames' #'comment')
]

{ #category : 'validation' }
RwPackageGroup >> validate [
	"ensure that the data structures within the receiver contain valid information:
		1. only packages defined in the receiver may be referenced in the reciever
		2. platform implementation is responsible for validating platform structures"

	self name ifNil: [ self error: 'name is nil' ].
	self condition ifNil: [ self error: 'name is nil' ].
	self _validatedPackageNames.
	^ true
]
