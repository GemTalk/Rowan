Extension { #name : 'ByteArray' }

{ #category : '*ston-gemstonecommon' }
ByteArray >> stonOn: stonWriter [
  "Use a hex representation"

  stonWriter writeObject: self listSingleton: self asHexString

]
