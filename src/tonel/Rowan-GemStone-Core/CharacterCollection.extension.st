Extension { #name : 'CharacterCollection' }

{ #category : '*rowan-gemstone-core' }
CharacterCollection >> asRwRepository [

	"return a platform appropriate repository for the repository identified in the receiver"

	^ self asRwUrl asRwRepository
]

{ #category : '*rowan-gemstone-core' }
CharacterCollection >> asRwUrl [

	""

	^ RwUrl fromString: self
]
