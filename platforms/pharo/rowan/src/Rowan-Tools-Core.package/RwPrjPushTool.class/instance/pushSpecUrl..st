smalltalk api
pushSpecUrl: aSpecUrlString
  ^ self
    pushSpecification: (RwSpecification fromUrl: aSpecUrlString)
    remote: nil