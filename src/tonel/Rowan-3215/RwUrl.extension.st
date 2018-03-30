Extension { #name : 'RwUrl' }

{ #category : '*rowan-3215' }
RwUrl class >> httpFromString: aString [
  ^CypressUrl absoluteFromText: aString
]
