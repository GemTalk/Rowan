Class {
	#name : 'RwImageSpecification',
	#superclass : 'RwSpecification',
	#instVars : [
		'loadedCommitId',
		'loadedConfigurationNames',
		'loadedGroupNames',
		'repositoryUrl'
	],
	#category : 'Rowan-Specifications'
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
RwImageSpecification >> loadedConfigurationNames [

	^ loadedConfigurationNames

]

{ #category : 'accessing' }
RwImageSpecification >> loadedConfigurationNames: aCollection [

	loadedConfigurationNames := aCollection

]

{ #category : 'accessing' }
RwImageSpecification >> loadedGroupNames [

	^ loadedGroupNames

]

{ #category : 'accessing' }
RwImageSpecification >> loadedGroupNames: aCollection [

	loadedGroupNames := aCollection

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
