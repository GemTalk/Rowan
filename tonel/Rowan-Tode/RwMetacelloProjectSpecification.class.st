Class {
	#name : 'RwMetacelloProjectSpecification',
	#superclass : 'RwProjectSpecification',
	#instVars : [
		'projectName',
		'loads',
		'locked'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'accessing' }
RwMetacelloProjectSpecification >> imageSpec [
  ^ imageSpec ifNil: [ imageSpec := RwProjectImageSpecification new ]
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

{ #category : 'accessing' }
RwMetacelloProjectSpecification >> projectName [

   ^projectName

]

{ #category : 'accessing' }
RwMetacelloProjectSpecification >> projectName: anObject [

   projectName := anObject

]
