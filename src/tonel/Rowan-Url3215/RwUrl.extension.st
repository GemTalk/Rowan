Extension { #name : 'RwUrl' }

{ #category : '*rowan-url3215' }
RwUrl class >> httpFromString: aString [
  ^CypressUrl absoluteFromText: aString
]
