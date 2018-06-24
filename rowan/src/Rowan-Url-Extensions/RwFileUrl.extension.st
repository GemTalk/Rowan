Extension { #name : 'RwFileUrl' }

{ #category : '*rowan-url-extensions' }
RwFileUrl >> asSpecification [
  "Answer an RwSpecification "

  ^ RwSpecification fromUrl: self

]
