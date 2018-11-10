Extension { #name : 'Symbol' }

{ #category : '*ast-kernel-core' }
Symbol >> rbStoreOn: aStream [
  aStream nextPut: $#.
  super rbStoreOn: aStream

]
