Extension { #name : 'ZnBufferedReadStream' }

{ #category : '*zinc-character-encoding-core-32x' }
ZnBufferedReadStream >> sizeBufferPatch9: size [
  "noop for 3.5.0 and beyond - still needed for 3.2.15"

	self sizeBuffer: size .
]


