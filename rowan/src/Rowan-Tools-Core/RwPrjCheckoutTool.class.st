Class {
	#name : 'RwPrjCheckoutTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'smalltalk api' }
RwPrjCheckoutTool >> checkoutSpecification: aRwSpecification [

	| checkout |
	self specification: aRwSpecification.
	checkout := specification repoSpec committish.
	checkout
		ifNil: [ 
			| msg |
			msg := 'No committish defined for the spec '
				, specification specName printString.
			self inform: msg.
			^ msg ]
		ifNotNil: [ 
			| gitTool gitRepoPath |
			gitTool := Rowan gitTools.
			gitRepoPath := specification repoSpec repositoryRootPath asFileReference.
			gitTool gitcheckoutIn: gitRepoPath with: checkout ].
	^ specification
]

{ #category : 'smalltalk api' }
RwPrjCheckoutTool >> checkoutSpecUrl: aString [ 

	^self checkoutSpecification: (RwSpecification fromUrl: aString) 

]
