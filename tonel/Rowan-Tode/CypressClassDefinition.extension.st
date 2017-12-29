Extension { #name : 'CypressClassDefinition' }

{ #category : '*rowan-tode' }
CypressClassDefinition >> accept: aVisitor [
	aVisitor visitClassDefinition: self.

]
