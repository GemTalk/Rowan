Class {
	#name : 'RwPlatformSubcomponent',
	#superclass : 'RwSubcomponent',
	#category : 'Rowan-Components'
}

{ #category : 'accessing' }
RwPlatformSubcomponent >> condition [
	^condition copy
]

{ #category : 'accessing' }
RwPlatformSubcomponent >> condition: anArray [
	anArray _isArray
		ifFalse: [ self error: 'The condition is constrained to be an array' ].
	condition := anArray
]

{ #category : 'accessing' }
RwPlatformSubcomponent >> conditionalPropertyMatchers [
	^ Dictionary new
		at:
				(self condition
						collect: [ :aCondition | self _platformPatternMatcherFor: aCondition ])
			put: {};
		yourself
]
