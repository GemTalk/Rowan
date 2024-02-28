Class {
	#name : 'RwDeleteClassFromSystemNotification',
	#superclass : 'RwNotification',
	#instVars : [
		'candidateClass'
	],
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwDeleteClassFromSystemNotification >> candidateClass [
	"class to be deleted from system, if receiver is #resumed: with true"

	^ candidateClass

]

{ #category : 'accessing' }
RwDeleteClassFromSystemNotification >> candidateClass: aClassToBeDeleted [

	candidateClass := aClassToBeDeleted

]

{ #category : 'handling' }
RwDeleteClassFromSystemNotification >> defaultAction [
	"delete class from system"

	^ true
]
