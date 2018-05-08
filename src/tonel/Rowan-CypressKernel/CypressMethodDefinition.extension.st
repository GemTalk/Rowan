Extension { #name : 'CypressMethodDefinition' }

{ #category : '*rowan-cypresskernel' }
CypressMethodDefinition >> accept: aVisitor [
	^ aVisitor visitMethodDefinition: self
]
