Class {
	#name : 'RwRepositorySpecification',
	#superclass : 'RwSpecification',
	#instVars : [
		'repositoryRootPath'
	],
	#category : 'Rowan-Specifications'
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

{ #category : 'initialization' }
RwRepositorySpecification >> initializeForExport [
  "if spec has been exported, clear out any of the fields that represent local disk state"

  super initializeForImport.
  repositoryRootPath := nil
]

{ #category : 'initialization' }
RwRepositorySpecification >> initializeForImport [
  "if spec has been imported, clear out any of the fields that represent local disk state"

  super initializeForImport.
  repositoryRootPath := nil
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
