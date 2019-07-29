Extension { #name : 'TestAsserter' }

{ #category : '*filesystem-gemstone-kernel' }
TestAsserter >> assert: anObject equals: otherObj description: aString [
	self
		assert: anObject = otherObj
		description: anObject printString , ' is not equal to ' , otherObj printString , ' - ' , aString.

]

{ #category : '*filesystem-gemstone-kernel' }
TestAsserter >> assertCollection: actual equals: expected [
	"Specialized test method that generates a proper error message for collection"
	^ self
		assert: expected = actual
		description: [ self comparingCollectionBetween: actual and: expected ]

]

{ #category : '*filesystem-gemstone-kernel' }
TestAsserter >> assertCollectionOfStrings: actual equalsCollectionOfStrings: expected [
	"Specialized test method that generates a proper error message for collection"
	actual with: expected do: [:a :b |
		self
			assert: (self isString: a equalToString: b)
			description: a printString , ' is  equal to ' , b printString]
]

{ #category : '*filesystem-gemstone-kernel' }
TestAsserter >> assertString: actual equalsString: expected [
	"Specialized test method that generates a proper error message for collection"
	^ self
		assert: (actual isEquivalent: expected)
		description: actual printString , ' is not equal to ' , expected printString
]

{ #category : '*filesystem-gemstone-kernel' }
TestAsserter >> comparingCollectionBetween: left and: right [
	| additionalLeft additionalRight sortBlock|
	
	"use a very slow sort block"
	sortBlock := [ :a :b | a asString <= b asString ].
	additionalLeft := (left difference: right) sort: sortBlock.
	additionalRight := (right difference: left) sort: sortBlock. 
	
	^ String streamContents: [:stream |
		stream
			nextPutAll: 'Given Collections do not match. Got '; lf;
			tab; nextPutAll: 'left := '; print: left; nextPut: $.; lf;
			nextPutAll: ' instead of ';
			tab; nextPutAll: ' right :='; print: right; nextPut: $.; lf.
		left size = right size
			ifFalse: [ 
				stream 
					nextPutAll: 'Collection size does not match: left='; 
					print: left size;
					nextPutAll: ' vs. right=';
					print: right size; lf ].
		additionalLeft isEmpty
			ifFalse: [ 
				stream 
					nextPutAll: 'Got ';
					print: additionalLeft size;
					nextPutAll: ' additional element(s) in the left collection: ';
					tab; print: additionalLeft  ].
		additionalRight isEmpty
			ifFalse: [ 
				stream 
					nextPutAll: 'Got ';
					print: additionalRight size;
					nextPutAll: ' additional element(s) in the right collection: ';
					tab; print: additionalRight  ]]
]

{ #category : '*filesystem-gemstone-kernel' }
TestAsserter >> isString: actual comparedToDifferentStringClass: expected [
	^(actual isKindOf: String) and: [
		(expected isKindOf: String) and: [
			actual class name ~~ expected class name]]
]

{ #category : '*filesystem-gemstone-kernel' }
TestAsserter >> isString: actual equalToString: expected [
	"Support testing strings in two different classes have the same contents"
	^(self isString: actual comparedToDifferentStringClass: expected)
		ifTrue: [
			actual size = expected size ifFalse: [^false].
			actual with: expected do: [:a :b | a == b ifFalse: [^false] ].
			true]
		ifFalse: [
			actual = expected]
]
