Extension { #name : 'CypressMethodDefinition' }

{ #category : '*rowan-cypress-kernel' }
CypressMethodDefinition >> accept: aVisitor [
	^ aVisitor visitMethodDefinition: self
]

{ #category : '*rowan-cypress-kernel' }
CypressMethodDefinition >> offset: anInteger inFile: aFileName [
	"noop"
]
