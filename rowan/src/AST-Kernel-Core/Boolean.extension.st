Extension { #name : 'Boolean' }

{ #category : '*ast-kernel-core' }
Boolean >> rbStoreOn: aStream [
  aStream nextPutAll: self asString

]
