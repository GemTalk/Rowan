Extension { #name : 'AbstractDictionary' }

{ #category : '*rowan-gemstone-kernel-36x' }
AbstractDictionary >> at: key ifPresent: aBlock [
   "Lookup the given key in the receiver. If it is present, answer the value of 
    evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| v |
	v := self at: key ifAbsent: [^ nil].
	^ aBlock cull: v
]
