Extension { #name : 'Utf8' }

{ #category : '*filesystem-gemstone-kernel' }
Utf8 >> asByteArray [
	^ ByteArray streamContents: [ :stream |
		self do: [ :each |
			stream nextPut: each ] ]
]

{ #category : '*filesystem-gemstone-kernel' }
Utf8 >> asString [
  "override the *filesystem  ByteArray >> asString"
  ^ self decodeToString   "or maybe  decodeToUnicode ??"
]
