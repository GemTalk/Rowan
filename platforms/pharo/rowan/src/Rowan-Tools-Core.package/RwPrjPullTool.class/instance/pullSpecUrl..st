smalltalk api
pullSpecUrl: aSpecUrlString
  ^ self
    pullSpecification: (RwSpecification fromUrl: aSpecUrlString)
    remote: nil