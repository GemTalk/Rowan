Extension { #name : 'ByteArray' }

{ #category : '*ston-core' }
ByteArray >> stonContainSubObjects [ 
	^ false

]

{ #category : '*ston-core' }
ByteArray class >> fromSton: stonReader [
  | singletonString |
  singletonString := stonReader parseListSingleton.
  ^ (self new: singletonString size // 2)
    readHexFrom: singletonString readStream

]
