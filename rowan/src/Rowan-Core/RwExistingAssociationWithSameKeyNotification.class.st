Class {
	#name : 'RwExistingAssociationWithSameKeyNotification',
	#superclass : 'RwNotification',
	#type : 'variable',
	#instVars : [
		'errorMessage'
	],
	#category : 'Rowan-Core'
}

{ #category : 'handling' }
RwExistingAssociationWithSameKeyNotification >> defaultAction [

	self error: self errorMessage
]

{ #category : 'accessing' }
RwExistingAssociationWithSameKeyNotification >> errorMessage [

   ^errorMessage

]

{ #category : 'accessing' }
RwExistingAssociationWithSameKeyNotification >> errorMessage: anObject [

   errorMessage := anObject

]
