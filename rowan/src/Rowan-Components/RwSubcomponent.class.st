Class {
	#name : 'RwSubcomponent',
	#superclass : 'RwLoadComponent',
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
			'Subcomponent cannot be independently loaded. The receiver is nested inside of load components'
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

{ #category : 'initialization' }
RwSubcomponent >> initialize [
	super initialize.
	condition := 'common'.
]

{ #category : 'ston' }
RwSubcomponent >> instVarNamesInOrderForSton [
	^ #(#'name' #'projectName' #'condition' #'preloadDoitName' #'postloadDoitName' #'projectNames' #'componentNames' #'packageNames' #'conditionalPackageMapSpecs' #'comment')
]
