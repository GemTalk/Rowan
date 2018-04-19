Extension { #name : 'STONWriter' }

{ #category : '*ston-gemstonebase-core' }
STONWriter >> writeFloat: float [
  writeStream nextPutAll: float asString

]

{ #category : '*ston-gemstonebase-core' }
STONWriter class >> new [
  ^ self basicNew
    initialize;
    yourself

]
