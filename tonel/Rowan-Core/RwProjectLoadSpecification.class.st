Class {
	#name : 'RwProjectLoadSpecification',
	#superclass : 'RwLoadSpecification',
	#instVars : [
		'projectName',
		'loads'
	],
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwProjectLoadSpecification >> loads [

   ^loads

]

{ #category : 'accessing' }
RwProjectLoadSpecification >> loads: anObject [

   loads := anObject

]

{ #category : 'loading' }
RwProjectLoadSpecification >> loadUsingLibrarian: aRwLibrarian [

	aRwLibrarian loadProjectSpecification: self
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
