Class {
	#name : 'RwGitRepositorySpecification',
	#superclass : 'RwRepositorySpecification',
	#instVars : [
		'committish',
		'committishType',
		'remote',
		'remoteUrl'
	],
	#category : 'Rowan-Specifications'
}

{ #category : 'accessing' }
RwGitRepositorySpecification >> branch: aString [

	self committish: aString committishType: 'branch'
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
RwGitRepositorySpecification >> initializeForExport [
  "if spec has been exported, clear out any of the fields that represent local disk state"

  super initializeForImport.
  remote := nil
]

{ #category : 'initialization' }
RwGitRepositorySpecification >> initializeForImport [
  "if spec has been imported, clear out any of the fields that represent local disk state"

  super initializeForImport.
  remote := nil
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

	^ remote ifNil: [ 'origin' ]
]

{ #category : 'accessing' }
RwGitRepositorySpecification >> remote: anObject [

   remote := anObject

]

{ #category : 'accessing' }
RwGitRepositorySpecification >> remoteUrl [

	^ remoteUrl
]

{ #category : 'accessing' }
RwGitRepositorySpecification >> remoteUrl: aRemoteUrlString [

	remoteUrl := aRemoteUrlString
]

{ #category : 'accessing' }
RwGitRepositorySpecification >> SHA: aString [

	self committish: aString committishType: 'SHA'
]

{ #category : 'accessing' }
RwGitRepositorySpecification >> tag: aString [

	self committish: aString committishType: 'tag'
]

{ #category : 'accessing' }
RwGitRepositorySpecification >> tagPattern: aString [

	self committish: aString committishType: 'tagPattern'
]

{ #category : 'actions' }
RwGitRepositorySpecification >> updateLoadedCommitIdForTool: aRwTool [
  ^ aRwTool doGitUpdateLoadedCommitId
]

{ #category : 'accessing' }
RwGitRepositorySpecification >> useGit [

	^ true
]

{ #category : 'accessing' }
RwGitRepositorySpecification >> version [

	| vers |
	vers :=  self versionString.
	vers isEmpty ifTrue: [ ^ nil ].
	^ vers asRwSemanticVersionNumber
]

{ #category : 'accessing' }
RwGitRepositorySpecification >> versionString [

	self committishType =  'tag'
		ifFalse: [ ^ '' ].
	self committish first = $v
		ifTrue: [ ^self committish copyFrom: 2 to: self committish size ].
	^self committish
]
