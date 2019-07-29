Extension { #name : 'STONWriter' }

{ #category : '*ston-gemstonebase' }
STONWriter >> writeFloat: float [
  writeStream nextPutAll: float asString

]

{ #category : '*ston-gemstonebase' }
STONWriter class >> new [
  ^ self basicNew
    initialize;
    yourself

]
