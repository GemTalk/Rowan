Extension { #name : 'RwFileUrl' }

{ #category : '*rowan-tode' }
RwFileUrl >> asRwRepository [
  ^ self createRwRepositoryForPath: self pathForDirectory
]
