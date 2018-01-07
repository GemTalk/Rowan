Class {
	#name : 'RwCheckoutTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'smalltalk api' }
RwCheckoutTool >> checkoutSpecification: aRwSpecification [
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
RwCheckoutTool >> checkoutSpecUrl: aString [ 

	^self checkoutSpecification: (RwSpecification fromUrl: aString) 

]
