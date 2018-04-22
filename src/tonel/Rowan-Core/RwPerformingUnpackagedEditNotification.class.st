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

	self inform: self informMessage

]

{ #category : 'accessing' }
RwPerformingUnpackagedEditNotification >> informMessage [

   ^informMessage

]

{ #category : 'accessing' }
RwPerformingUnpackagedEditNotification >> informMessagee: anObject [

   informMessage := anObject

]
