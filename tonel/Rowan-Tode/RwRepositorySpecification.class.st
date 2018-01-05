Class {
	#name : 'RwRepositorySpecification',
	#superclass : 'RwSpecification',
	#category : 'Rowan-Tode'
}

{ #category : 'testing' }
RwRepositorySpecification >> canCommit [
  ^ false
]

{ #category : 'actions' }
RwRepositorySpecification >> commitForTool: aRwTool message: messageString [
  "commit not supported by default"

  
]
