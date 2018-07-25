Class {
	#name : 'RwDeleteClassFromSystemNotification',
	#superclass : 'RwNotification',
	#type : 'variable',
	#instVars : [
		'candidateClass'
	],
	#gs_options : [
		'disallowGciStore'
	],
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwDeleteClassFromSystemNotification >> candidateClass [

	^ candidateClass

]

{ #category : 'accessing' }
RwDeleteClassFromSystemNotification >> candidateClass: aClass [

	candidateClass := aClass

]

{ #category : 'handling' }
RwDeleteClassFromSystemNotification >> defaultAction [

	^ true

]
