Extension { #name : 'CypressMethodDefinition' }

{ #category : '*rowan-cypress' }
CypressMethodDefinition >> accept: aVisitor [
	^ aVisitor visitMethodDefinition: self
]
