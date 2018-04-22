Class {
	#name : 'RwPerformingUnpackagedEditNotification',
	#superclass : 'RwNotification',
	#type : 'variable',
	#instVars : [
		'informMessage'
	],
	#category : 'Rowan-Core'
}

{ #category : 'handling' }
RwPerformingUnpackagedEditNotification >> defaultAction [

	^ self error: self errorMessage

]

{ #category : 'accessing' }
RwPerformingUnpackagedEditNotification >> errorMessage [

   ^self informMessage

]

{ #category : 'accessing' }
RwPerformingUnpackagedEditNotification >> informMessage [

   ^informMessage

]

{ #category : 'accessing' }
RwPerformingUnpackagedEditNotification >> informMessage: anObject [

   informMessage := anObject

]

{ #category : 'signalling' }
RwPerformingUnpackagedEditNotification >> signal: aMessage [

	self informMessage: aMessage.
	^ self signal

]
