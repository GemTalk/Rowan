Extension { #name : 'QuadByteString' }

{ #category : '*FileSystem-GemStone-Kernel' }
QuadByteString class >> from: aString [ 

	| newString |
	(aString isMemberOf: self)
		ifTrue: [^ aString copy].
	newString := self new: aString size.
	1 to: aString size do: [:index | newString at: index put: (aString at: index)].
	^ newString
]
