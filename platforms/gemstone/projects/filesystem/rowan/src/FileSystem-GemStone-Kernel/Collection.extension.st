Extension { #name : 'Collection' }

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
Collection >> isEmptyOrNil [
  "Answer whether the receiver contains any elements, or is nil.  Useful in numerous situations where one wishes the same reaction to an empty collection or to nil"

  ^ self size == 0

]

{ #category : '*filesystem-gemstone-kernel' }
Collection >> sort [

	"Sort this array into ascending order using the '<=' operator."

	^ self sort: [ :a :b | a <= b ]

]
