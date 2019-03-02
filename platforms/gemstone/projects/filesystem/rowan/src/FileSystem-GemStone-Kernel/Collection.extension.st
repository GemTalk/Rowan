Extension { #name : 'Collection' }

{ #category : '*filesystem-gemstone-kernel' }
Collection >> atRandom [
	"Answer a random element of the receiver.  Uses a shared random 
	number generator owned by class Collection.  If you use this a lot, 
	define your own instance of Random and use #atRandom:.  Causes 
	an error if self has no elements."

	^ self atRandom: Random new.

"Examples:
	#('one' 'or' 'the' 'other') atRandom
	(1 to: 10) atRandom
	'Just pick one of these letters at random' atRandom
	#(3 7 4 9 21) asSet atRandom		(just to show it also works for Sets)
"
]

{ #category : '*filesystem-gemstone-kernel' }
Collection >> atRandom: aGenerator [
	"Answer a random element of the receiver. Uses aGenerator which
    should be kept by the user in a variable and used every time. Use
    this instead of #atRandom for better uniformity of random numbers because 
	only you use the generator. Causes an error if self has no elements."
	| rand index |

	self emptyCheck.
	rand := aGenerator nextInt: self size.
	index := 1.
	self do: [:each |
		index = rand ifTrue: [^each].
		index := index + 1].
	^ self errorEmptyCollection
]

{ #category : '*filesystem-gemstone-kernel' }
Collection >> collect: aBlock as: aClass [
	"Evaluate aBlock with each of the receiver's elements as the argument.  
	Collect the resulting values into an instance of aClass. Answer the resulting collection."

	^(aClass new: self size) fillFrom: self with: aBlock
]

{ #category : '*filesystem-gemstone-kernel' }
Collection >> difference: aCollection [
  "Answer the set theoretic difference of two collections."

  ^ self reject: [ :each | aCollection includes: each ]
]

{ #category : '*filesystem-gemstone-kernel' }
Collection >> fillFrom: aCollection with: aBlock [
	"Evaluate aBlock with each of aCollections's elements as the argument.  
	Collect the resulting values into self. Answer self."

	aCollection do: [ :each |
		self add: (aBlock value: each) ]
]

{ #category : '*filesystem-gemstone-kernel' }
Collection >> ifEmpty: aBlock [
  self size == 0
    ifTrue: [ ^ aBlock value ]

]

{ #category : '*filesystem-gemstone-kernel' }
Collection >> ifEmpty: emptyBlock ifNotEmpty: notEmptyBlock [
	"Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise"
	" If the notEmptyBlock has an argument, eval with the receiver as its argument"

	self isEmpty ifTrue: [ ^emptyBlock value ].
	^notEmptyBlock cull: self
]

{ #category : '*filesystem-gemstone-kernel' }
Collection >> ifNotEmpty: aBlock [
	"Evaluate the given block with the receiver as argument, answering its value
    unless the receiver is empty, in which case answer the receiver."

    ^self isEmpty
          ifTrue: [self]
          ifFalse: [aBlock cull: self]
]

{ #category : '*filesystem-gemstone-kernel' }
Collection >> ifNotEmpty: notEmptyBlock ifEmpty: emptyBlock [
	"Evaluate emptyBlock if I'm empty, notEmptyBlock otherwise
	 If the notEmptyBlock has an argument, eval with the receiver as its argument"

	self isEmpty ifFalse: [ ^notEmptyBlock cull: self ].
	^emptyBlock value
]

{ #category : '*filesystem-gemstone-kernel' }
Collection >> isEmptyOrNil [
  "Answer whether the receiver contains any elements, or is nil.  Useful in numerous situations where one wishes the same reaction to an empty collection or to nil"

  ^ self size == 0

]

{ #category : '*filesystem-gemstone-kernel' }
Collection >> noneSatisfy: aBlock [
	"Evaluate aBlock with the elements of the receiver. If aBlock returns false for all elements return true. Otherwise return false"

	self do: [:item | (aBlock value: item) ifTrue: [^ false]].
	^ true
]

{ #category : '*filesystem-gemstone-kernel' }
Collection >> removeAll [

	self removeAll: self
]

{ #category : '*filesystem-gemstone-kernel' }
Collection >> sort [

	"Sort this array into ascending order using the '<=' operator."

	^ self sort: [ :a :b | a <= b ]

]
