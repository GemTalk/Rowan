Extension { #name : 'CharacterCollection' }

{ #category : '*Rowan-Tode' }
CharacterCollection >> asRwRepository [

	"return a platform appropriate repository for the repository identified in the receiver"

	^ self asRwUrl asRwRepository
]

{ #category : '*Rowan-Tode' }
CharacterCollection >> asRwUrl [

	""

	^ RwUrl fromString: self
]
