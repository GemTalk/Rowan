Class {
	#name : 'RwProjectLoadSpecification',
	#superclass : 'RwLoadSpecification',
	#instVars : [
		'projectName',
		'loads',
		'locked'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'accessing' }
RwProjectLoadSpecification >> loads [

   ^loads

]

{ #category : 'accessing' }
RwProjectLoadSpecification >> loads: anObject [

   loads := anObject

]

{ #category : 'accessing' }
RwProjectLoadSpecification >> locked [

   ^locked

]

{ #category : 'accessing' }
RwProjectLoadSpecification >> locked: anObject [

   locked := anObject

]

{ #category : 'actions' }
RwProjectLoadSpecification >> packageNamesForTool: aRwTool [
  ^ aRwTool packageNamesForProjectSpec: self
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> packageOrGroupNamesToLoad: packageOrGroupNames [

	loads := packageOrGroupNames
]

{ #category : 'accessing' }
RwProjectLoadSpecification >> projectName [

   ^projectName

]

{ #category : 'accessing' }
RwProjectLoadSpecification >> projectName: anObject [

   projectName := anObject

]
