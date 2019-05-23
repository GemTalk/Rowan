smalltalk api
pushSpecUrl: aSpecUrlString remote: remote
  ^ self
    pushSpecification: (RwSpecification fromUrl: aSpecUrlString)
    remote: remote