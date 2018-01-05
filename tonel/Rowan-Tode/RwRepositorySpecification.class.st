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

{ #category : 'actions' }
RwRepositorySpecification >> commitLogForTool: aRwTool limit: logLimit [
  "commit log not supported by default"

  
]

{ #category : 'actions' }
RwRepositorySpecification >> pullForTool: aRwTool remote: remoteOrNil [
  "pull not supported by default"

  
]

{ #category : 'actions' }
RwRepositorySpecification >> pushForTool: aRwTool remote: remoteOrNil [
  "push not supported by default"

  
]
