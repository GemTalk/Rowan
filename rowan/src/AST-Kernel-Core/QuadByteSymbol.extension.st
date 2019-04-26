Extension { #name : 'QuadByteSymbol' }

{ #category : '*ast-kernel-core' }
QuadByteSymbol >> rbStoreOn: aStream [
  aStream nextPut: $#.
  super rbStoreOn: aStream

]
