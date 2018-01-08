Extension { #name : 'CypGenericUrl' }

{ #category : '*rowan-tode' }
CypGenericUrl >> asZnUrl [
	^ ZnUrl fromString: self printString
]
