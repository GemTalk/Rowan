Class {
	#name : 'RwImageSpecification',
	#superclass : 'RwSpecification',
	#instVars : [
		'loadedCommitId'
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
