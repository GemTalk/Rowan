Extension { #name : 'RwProjectSpecificationV2' }

{ #category : '*rowan-definitionsv2' }
RwProjectSpecificationV2 >> asProjectDefiniton [
	^ RwProjectDefinitionV2 fromLoadSpecification: self
]

{ #category : '*rowan-definitionsv2' }
RwProjectSpecificationV2 >> asProjectDefinitonWithComment: commentString [
	^ (RwProjectDefinitionV2 fromLoadSpecification: self)
		comment: commentString;
		yourself
]
