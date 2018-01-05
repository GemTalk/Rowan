Class {
	#name : 'RwGitRepositorySpecification',
	#superclass : 'RwRepositorySpecification',
	#instVars : [
		'commitish',
		'commitishType',
		'repositoryRootPath',
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
RwGitRepositorySpecification >> commitish [

   ^commitish

]

{ #category : 'accessing' }
RwGitRepositorySpecification >> commitish: anObject [

   commitish := anObject

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

{ #category : 'initialization' }
RwGitRepositorySpecification >> initialize [

	super initialize.
	commitish := 'master'.
	commitishType := 'branch'

]

{ #category : 'initialization' }
RwGitRepositorySpecification >> initializeForHttpsDownload [
  "if spec has been downloaded, clear out any of the fields that represent local disk state"

  super initializeForHttpsDownload.
  remote := repositoryRootPath := nil
]

{ #category : 'actions' }
RwGitRepositorySpecification >> pullForTool: aRwTool remote: remoteString [
  ^ aRwTool doGitPull: remoteString
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
RwGitRepositorySpecification >> repositoryRootPath [

   ^repositoryRootPath

]

{ #category : 'accessing' }
RwGitRepositorySpecification >> repositoryRootPath: anObject [

   repositoryRootPath := anObject

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
