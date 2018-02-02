Extension { #name : 'CharacterCollection' }

{ #category : '*rowan-tode' }
CharacterCollection >> asRwUrl [
  ^ RwUrl fromString: self
]
{ #category : '*rowan-tode' }
CharacterCollection >> asRwRepository [

	"return a platform appropriate repository for the repository identified in the receiver"

	^ self asRwUrl asRwRepository
]
