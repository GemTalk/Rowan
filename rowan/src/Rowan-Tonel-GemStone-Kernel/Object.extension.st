Extension { #name : 'Object' }

{ #category : '*rowan-tonel-gemstone-kernel' }
Object >> isCharacter [

	^ false

]

{ #category : '*rowan-tonel-gemstone-kernel' }
Object >> isCollection [

	"Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:"

	^ false

]

{ #category : '*rowan-tonel-gemstone-kernel' }
Object >> makeSparkle [
	(self isSpecial or:[self isKindOf: String])
		ifTrue: [ ^ self ].
	(self isKindOf: SequenceableCollection) ifTrue:[^self copy collect:[:element | element makeSparkle]].
	^ self printString
]

{ #category : '*rowan-tonel-gemstone-kernel' }
Object >> putOn: aStream [

	^ aStream nextPut: self

]
