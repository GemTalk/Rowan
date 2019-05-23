smalltalk api
pullSpecUrl: aSpecUrlString remote: remote
  ^ self
    pullSpecification: (RwSpecification fromUrl: aSpecUrlString)
    remote: remote