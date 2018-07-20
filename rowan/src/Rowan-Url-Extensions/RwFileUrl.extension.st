Extension { #name : 'RwFileUrl' }

{ #category : '*rowan-url-extensions' }
RwFileUrl >> asConfiguration [
  "Answer a concrete RwAbstractProjectConfiguration "

  ^ RwAbstractProjectConfiguration fromUrl: self

]

{ #category : '*rowan-url-extensions' }
RwFileUrl >> asSpecification [
  "Answer an RwSpecification "

  ^ RwSpecification fromUrl: self

]
