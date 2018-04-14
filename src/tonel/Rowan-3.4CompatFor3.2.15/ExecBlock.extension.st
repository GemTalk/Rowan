Extension { #name : 'ExecBlock' }

{ #category : '*rowan-3.4compatfor3.2.15' }
ExecBlock >> cull: anArg [

	"Return the value of the receiver evaluated with 0 or 1 arguments. If the block
   expects 1 argument pass anArg as the value of the argument"

	^ self argumentCount == 0
		ifTrue: [ self value ]
		ifFalse: [ self value: anArg ]

]

{ #category : '*rowan-3.4compatfor3.2.15' }
ExecBlock >> cull: firstArg cull: secondArg [

	"Return the value of the receiver evaluated with between 0 and 2 arguments,
   discarding arguments not needed by the receiver."

	| nargs |
	(nargs := self argumentCount) < 2
		ifTrue: [ 
			nargs == 1
				ifTrue: [ ^ self value: firstArg ].
			^ self value ].
	^ self value: firstArg value: secondArg

]

{ #category : '*rowan-3.4compatfor3.2.15' }
ExecBlock >> cull: firstArg cull: secondArg cull: thirdArg [

	"Return the value of the receiver evaluated with between 0 and 3 arguments,
   discarding arguments not needed by the receiver."

	| nargs |
	(nargs := self argumentCount) < 3
		ifTrue: [ 
			nargs == 2
				ifTrue: [ ^ self value: firstArg value: secondArg ].
			nargs == 1
				ifTrue: [ ^ self value: firstArg ].
			^ self value ].
	^ self value: firstArg value: secondArg value: thirdArg

]

{ #category : '*rowan-3.4compatfor3.2.15' }
ExecBlock >> cull: firstArg cull: secondArg cull: thirdArg cull: fourthArg [

	"Return the value of the receiver evaluate with between 0 and 4 arguments,
   discarding arguments not needed by the receiver."

	| nargs |
	(nargs := self argumentCount) <= 2
		ifTrue: [ 
			nargs == 2
				ifTrue: [ ^ self value: firstArg value: secondArg ].
			nargs == 1
				ifTrue: [ ^ self value: firstArg ].
			^ self value ].
	nargs == 3
		ifTrue: [ ^ self value: firstArg value: secondArg value: thirdArg ].
	^ self
		value: firstArg
		value: secondArg
		value: thirdArg
		value: fourthArg

]
