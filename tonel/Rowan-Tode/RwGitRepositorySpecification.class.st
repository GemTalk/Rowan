Class {
	#name : 'RwGitRepositorySpecification',
	#superclass : 'RwRepositorySpecification',
	#instVars : [
		'commitish',
		'commitishType'
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
  "commit not supported by default"

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

{ #category : 'initialization' }
RwGitRepositorySpecification >> initialize [

	super initialize.
	commitish := 'master'.
	commitishType := 'branch'

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
