Extension { #name : 'RwUrl' }

{ #category : '*rowan-url-3215' }
RwUrl class >> httpFromString: aString [
  ^CypressUrl absoluteFromText: aString
]
