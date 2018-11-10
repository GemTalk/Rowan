Extension { #name : 'DoubleByteSymbol' }

{ #category : '*ast-kernel-core' }
DoubleByteSymbol >> rbStoreOn: aStream [
  aStream nextPut: $#.
  super rbStoreOn: aStream

]
