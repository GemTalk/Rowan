Class {
	#name : 'RwComponentLoadConfiguration',
	#superclass : 'RwProjectLoadConfiguration',
	#instVars : [
		'projectNames'
	],
	#category : 'Rowan-Components'
}

{ #category : 'visiting' }
RwComponentLoadConfiguration >> acceptVisitor: aVisitor [

	^aVisitor visitComponentLoadConfiguration: self
]

{ #category : 'accessing' }
RwComponentLoadConfiguration >> projectNames [

	^ projectNames ifNil: [ #() ]
]

{ #category : 'accessing' }
RwComponentLoadConfiguration >> projectNames: anArray [

	projectNames := anArray
]
