Class {
	#name : 'RwImageSpecification',
	#superclass : 'RwSpecification',
	#instVars : [
		'loadedCommitId',
		'repositoryUrl'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'accessing' }
RwImageSpecification >> loadedCommitId [
  ^ loadedCommitId
]

{ #category : 'accessing' }
RwImageSpecification >> loadedCommitId: anObject [
  loadedCommitId := anObject
]

{ #category : 'accessing' }
RwImageSpecification >> repositoryUrl [

   ^repositoryUrl

]

{ #category : 'accessing' }
RwImageSpecification >> repositoryUrl: anObject [

   repositoryUrl := anObject

]

{ #category : 'actions' }
RwImageSpecification >> updateLoadedPackageNamesTool: aRwTool packageNames: aPackageNames [
  "noop by default"

  
]
