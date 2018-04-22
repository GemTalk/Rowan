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

	^ self confirm: self informMessage

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
