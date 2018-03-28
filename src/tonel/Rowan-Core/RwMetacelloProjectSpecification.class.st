Class {
	#name : 'RwMetacelloProjectSpecification',
	#superclass : 'RwProjectSpecification',
	#instVars : [
		'projectName',
		'loads',
		'locked'
	],
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwMetacelloProjectSpecification >> imageSpec [

	^ imageSpec ifNil: [ imageSpec := RwMetacelloProjectImageSpecification new ]
]

{ #category : 'accessing' }
RwMetacelloProjectSpecification >> loads [

   ^loads

]

{ #category : 'accessing' }
RwMetacelloProjectSpecification >> loads: anObject [

   loads := anObject

]

{ #category : 'accessing' }
RwMetacelloProjectSpecification >> locked [

   ^locked

]

{ #category : 'accessing' }
RwMetacelloProjectSpecification >> locked: anObject [

   locked := anObject

]

{ #category : 'actions' }
RwMetacelloProjectSpecification >> packageNamesForTool: aRwTool [
  ^ aRwTool packageNamesForProjectSpec: self
]

{ #category : 'accessing' }
RwMetacelloProjectSpecification >> packageOrGroupNamesToLoad: packageOrGroupNames [

	loads := packageOrGroupNames
]

{ #category : 'copying' }
RwMetacelloProjectSpecification >> postCopy [
  "don't forget to copy any compound state objects"

  super postCopy.
  loads := loads copy
]

{ #category : 'accessing' }
RwMetacelloProjectSpecification >> projectName [

   ^projectName

]

{ #category : 'accessing' }
RwMetacelloProjectSpecification >> projectName: anObject [

   projectName := anObject

]
