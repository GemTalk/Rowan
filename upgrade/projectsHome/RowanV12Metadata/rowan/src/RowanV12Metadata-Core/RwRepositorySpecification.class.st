Class {
	#name : 'RwRepositorySpecification',
	#superclass : 'RwSpecification',
	#instVars : [
		'repositoryRootPath'
	],
	#category : 'RowanV12Metadata-Core'
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

{ #category : 'copying' }
RwRepositorySpecification >> postCopy [
  "don't forget to copy any compound state objects"

  super postCopy
]

{ #category : 'actions' }
RwRepositorySpecification >> pullForTool: aRwTool remote: remoteOrNil [
  "pull not supported by default"

  
]

{ #category : 'actions' }
RwRepositorySpecification >> pushForTool: aRwTool remote: remoteOrNil [
  "push not supported by default"

  
]

{ #category : 'accessing' }
RwRepositorySpecification >> repositoryRootPath [

   ^repositoryRootPath

]

{ #category : 'accessing' }
RwRepositorySpecification >> repositoryRootPath: anObject [

   repositoryRootPath := anObject

]

{ #category : 'actions' }
RwRepositorySpecification >> updateLoadedCommitIdForTool: aRwTool [
  "commit not supported by default"

  
]

{ #category : 'accessing' }
RwRepositorySpecification >> useGit [

	^ false
]
