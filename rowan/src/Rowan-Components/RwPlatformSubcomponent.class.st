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

{ #category : 'accessing' }
RwPlatformSubcomponent >> label [
	| strm |
	strm := WriteStream on: String new.
	strm
		nextPutAll: self basename;
		space;
		nextPut: ${.
	self condition
		do: [ :cond | 
			strm
				space;
				nextPutAll: cond asString ].
	strm
		space;
		nextPut: $}.
	^ strm contents
]
