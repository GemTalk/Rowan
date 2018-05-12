Class {
	#name : 'RwRepCommitTool',
	#superclass : 'RwRepositoryTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'smalltalk api' }
RwRepCommitTool >> commitRepositoryForSpecification: aRwSpecification message: messageString [
  self specification: aRwSpecification.
  specification canCommit
    ifFalse: [ 
      | msg |
      msg := 'repository for ' , specification specName printString
        ,
          ' does not support commit operations. Source written to repository and skipping commit'.
      self inform: msg.
      ^ msg ].
  specification commitForTool: self message: messageString.
  ^ specification updateLoadedCommitIdForTool: self
]

{ #category : 'smalltalk api' }
RwRepCommitTool >> commitRepositoryForSpecUrl: aSpecUrlString message: messageString [
  "commit repository"

  ^ self
    commitRepositoryForSpecification: (RwSpecification fromUrl: aSpecUrlString)
    message: messageString
]
