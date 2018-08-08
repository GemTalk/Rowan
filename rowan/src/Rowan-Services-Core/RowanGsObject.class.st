Class {
	#name : 'RowanGsObject',
	#superclass : 'RowanCommandResult',
	#instVars : [
		'gciSession',
		'name',
		'oopType'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'other' }
RowanGsObject >> name [

	^name

]

{ #category : 'other' }
RowanGsObject >> name: anObject [

	name := anObject

]

{ #category : 'other' }
RowanGsObject >> oopType [

	^oopType

]

{ #category : 'other' }
RowanGsObject >> oopType: anObject [

	oopType := anObject

]
