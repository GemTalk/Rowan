Extension { #name : 'CharacterCollection' }

{ #category : '*rowan-gemstone' }
CharacterCollection >> asRwRepository [

	"return a platform appropriate repository for the repository identified in the receiver"

	^ self asRwUrl asRwRepository
]

{ #category : '*rowan-gemstone' }
CharacterCollection >> asRwUrl [

	""

	^ RwUrl fromString: self
]
