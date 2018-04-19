Extension { #name : 'RwUrl' }

{ #category : '*rowan-gemstone-3215' }
RwUrl class >> httpFromString: aString [
  ^CypressUrl absoluteFromText: aString
]
