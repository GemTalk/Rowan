Extension { #name : 'UndefinedObject' }

{ #category : '*ast-kernel-core' }
UndefinedObject >> rbStoreOn: aStream [
  aStream nextPutAll: self asString

]
