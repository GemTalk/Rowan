Extension { #name : 'Collection' }

{ #category : '*filesystem-gemstone-kernel' }
Collection >> difference: aCollection [
  "Answer the set theoretic difference of two collections."

  ^ self reject: [ :each | aCollection includes: each ]
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
