Class {
	#name : 'RwImageSpecification',
	#superclass : 'RwSpecification',
	#instVars : [
		'loadedCommitId',
		'repositoryUrl'
	],
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwImageSpecification >> loadedCommitId [

	^ loadedCommitId ifNil: [ '' ]
]

{ #category : 'accessing' }
RwImageSpecification >> loadedCommitId: anObject [
  loadedCommitId := anObject
]

{ #category : 'accessing' }
RwImageSpecification >> loadedPackages [

	^ self subclassResponsibility
]

{ #category : 'copying' }
RwImageSpecification >> postCopy [
  "don't forget to copy any compound state objects"

  super postCopy
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
