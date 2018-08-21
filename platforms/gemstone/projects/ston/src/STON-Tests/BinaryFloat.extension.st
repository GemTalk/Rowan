Extension { #name : 'BinaryFloat' }

{ #category : '*ston-tests' }
BinaryFloat >> closeTo: num [
  "are these two numbers close?"

  num _isNumber
    ifFalse: [ ^ [ self = num ] on: Error do: [:ignored | false ] ].
  self = 0.0
    ifTrue: [ ^ num abs < 0.0001 ].
  num = 0
    ifTrue: [ ^ self abs < 0.0001 ].
  ^ self = num asFloat
    or: [ (self - num) abs / (self abs max: num abs) < 0.0001 ]

]
