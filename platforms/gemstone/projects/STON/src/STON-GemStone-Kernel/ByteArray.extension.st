Extension { #name : 'ByteArray' }

{ #category : '*ston-gemstone-kernel' }
ByteArray >> stonContainSubObjects [ 
	^ false

]

{ #category : '*ston-gemstone-kernel' }
ByteArray class >> fromSton: stonReader [
  | singletonString |
  singletonString := stonReader parseListSingleton.
  ^ (self new: singletonString size // 2)
    readHexFrom: singletonString readStream

]
