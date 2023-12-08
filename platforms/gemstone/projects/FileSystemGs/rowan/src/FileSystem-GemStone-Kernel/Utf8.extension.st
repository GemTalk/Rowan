Extension { #name : 'Utf8' }

{ #category : '*filesystem-gemstone-kernel' }
Utf8 >> asByteArray [
	"used to keep the tests portable between GemStone and Pharo - may not be required long term?"
	^ ByteArray streamContents: [ :stream |
		self do: [ :each |
			stream nextPut: each ] ]
]
