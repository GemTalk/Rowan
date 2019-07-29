Extension { #name : 'Object' }

{ #category : '*Zinc-Character-Encoding-Core' }
Object >> isGemStoneUtf8Encoding [
	"This is for testing only"
	self flag: 'GemStone/Pharo code switched for research'.


	" Set to Encoder Type To Pharo Style: ' SessionTemps current at: #ZnEncoding put: #Pharo ' " 
	" Set to Encoder Type To GemStone Style: ' SessionTemps current at: #ZnEncoding put: #GemStone ' " 

	^(SessionTemps current at: #ZnEncoding otherwise: #GemStone) = #GemStone
]
