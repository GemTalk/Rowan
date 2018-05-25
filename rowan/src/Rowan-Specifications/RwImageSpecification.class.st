Class {
	#name : 'RwImageSpecification',
	#superclass : 'RwSpecification',
	#instVars : [
		'loadedCommitId',
		'loadedConfigurationName',
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
RwImageSpecification >> loadedConfigurationName [

	^ loadedConfigurationName

]

{ #category : 'accessing' }
RwImageSpecification >> loadedConfigurationName: aString [

	loadedConfigurationName := aString

]

{ #category : 'accessing' }
RwImageSpecification >> loadedGroupNames [

	^ loadedGroupNames

]

{ #category : 'accessing' }
RwImageSpecification >> loadedGroupNames: aCollection [

	loadedGroupNames := aCollection

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
