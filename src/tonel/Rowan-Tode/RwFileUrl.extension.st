Extension { #name : 'RwFileUrl' }

{ #category : '*rowan-tode' }
RwFileUrl >> asRwRepository [
  "return a platform appropriate repository for the repository located in the directory referenced by the receiver"

  ^ self createRwRepositoryForPath: self pathForDirectory
]
