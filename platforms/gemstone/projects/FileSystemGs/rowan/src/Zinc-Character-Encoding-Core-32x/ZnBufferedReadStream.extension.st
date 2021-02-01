Extension { #name : 'ZnBufferedReadStream' }

{ #category : '*zinc-character-encoding-core-32x' }
ZnBufferedReadStream >> sizeBuffer: size [
	buffer := self collectionSpecies new: size .
]


