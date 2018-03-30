Extension { #name : 'RwUrl' }

{ #category : '*rowan-tode' }
RwUrl >> asZnUrl [
  ^ ZnUrl fromString: self printString
]

{ #category : '*rowan-tode' }
RwUrl class >> httpFromString: aString [
  ^ ZnUrl fromString: aString
]
