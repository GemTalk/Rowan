Class {
	#name : 'RwGitRepositorySpecification',
	#superclass : 'RwRepositorySpecification',
	#instVars : [
		'committish',
		'committishType',
		'remote'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'accessing' }
RwGitRepositorySpecification >> branch: aString [

	self commitish: aString.
	self commitishType: 'branch'
]

{ #category : 'testing' }
RwGitRepositorySpecification >> canCommit [
  ^ true
]

{ #category : 'actions' }
RwGitRepositorySpecification >> commitForTool: aRwTool message: messageString [
  ^ aRwTool doGitCommit: messageString
]

{ #category : 'accessing' }
RwGitRepositorySpecification >> commitishType [
	" branch, SHA, tag, tagPattern"

   ^commitishType

]

{ #category : 'accessing' }
RwGitRepositorySpecification >> commitishType: anObject [

   commitishType := anObject

]

{ #category : 'actions' }
RwGitRepositorySpecification >> commitLogForTool: aRwTool limit: logLimit [
  ^ aRwTool doGitCommitLog: logLimit
]

{ #category : 'accessing' }
RwGitRepositorySpecification >> committish [
  ^ committish
]

{ #category : 'accessing' }
RwGitRepositorySpecification >> committish: anObject [
  committish := anObject
]

{ #category : 'accessing' }
RwGitRepositorySpecification >> committishType [
  " branch, SHA, tag, tagPattern"

  ^ committishType
]

{ #category : 'accessing' }
RwGitRepositorySpecification >> committishType: anObject [
  committishType := anObject
]

{ #category : 'initialization' }
RwGitRepositorySpecification >> initialize [
  super initialize.
  committish := 'master'.
  committishType := 'branch'
]

{ #category : 'initialization' }
RwGitRepositorySpecification >> initializeForImport [
  "if spec has been imported, clear out any of the fields that represent local disk state"

  super initializeForImport.
  remote := repositoryRootPath := nil
]

{ #category : 'copying' }
RwGitRepositorySpecification >> postCopy [
  "don't forget to copy any compound state objects"

  super postCopy
]

{ #category : 'actions' }
RwGitRepositorySpecification >> pullForTool: aRwTool remote: remoteString [
  ^ aRwTool doGitPull: remoteString
]

{ #category : 'actions' }
RwGitRepositorySpecification >> pushForTool: aRwTool remote: remoteString [
  ^ aRwTool doGitPush: remoteString
]

{ #category : 'accessing' }
RwGitRepositorySpecification >> remote [
  ^ remote ifNil: [ '' ]
]

{ #category : 'accessing' }
RwGitRepositorySpecification >> remote: anObject [

   remote := anObject

]

{ #category : 'accessing' }
RwGitRepositorySpecification >> SHA: aString [

	self commitish: aString.
	self commitishType: 'SHA'
]

{ #category : 'accessing' }
RwGitRepositorySpecification >> tag: aString [

	self commitish: aString.
	self commitishType: 'tag'
]

{ #category : 'accessing' }
RwGitRepositorySpecification >> tagPattern: aString [

	self commitish: aString.
	self commitishType: 'tagPattern'
]

{ #category : 'actions' }
RwGitRepositorySpecification >> updateLoadedCommitIdForTool: aRwTool [
  ^ aRwTool doGitUpdateLoadedCommitId
]

{ #category : 'accessing' }
RwGitRepositorySpecification >> useGit [

	^ true
]
