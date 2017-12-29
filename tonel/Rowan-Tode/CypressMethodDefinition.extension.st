Extension { #name : 'CypressMethodDefinition' }

{ #category : '*rowan-tode' }
CypressMethodDefinition >> accept: aVisitor [
	^ aVisitor visitMethodDefinition: self
]
