Extension { #name : 'Number' }

{ #category : '*ston-tests' }
Number >> closeTo: num [
  "are these two numbers close?"

  num _isFloat
    ifTrue: [ ^ num closeTo: self asFloat ].
  ^ [ self = num ] on: Error do: [:ignored |  false ]

]
