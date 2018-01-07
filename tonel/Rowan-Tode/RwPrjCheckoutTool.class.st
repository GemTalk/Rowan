Class {
	#name : 'RwPrjCheckoutTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tode'
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
      | gitTool gitRepoDir |
      gitTool := self class git.
      gitRepoDir := ServerFileDirectory
        on: specification repoSpec repositoryRootPath.
      gitTool gitcheckoutIn: gitRepoDir with: checkout ].
  ^ specification
]

{ #category : 'smalltalk api' }
RwPrjCheckoutTool >> checkoutSpecUrl: aString [ 

	^self checkoutSpecification: (RwSpecification fromUrl: aString) 

]
