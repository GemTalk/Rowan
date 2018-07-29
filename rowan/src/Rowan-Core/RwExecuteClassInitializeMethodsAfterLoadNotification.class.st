Class {
	#name : 'RwExecuteClassInitializeMethodsAfterLoadNotification',
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
RwExecuteClassInitializeMethodsAfterLoadNotification >> candidateClass [
	"class to which #initialize if receiver is #resumed: with true"

	^ candidateClass

]

{ #category : 'accessing' }
RwExecuteClassInitializeMethodsAfterLoadNotification >> candidateClass: aClassToInitialize [

	candidateClass := aClassToInitialize

]

{ #category : 'handling' }
RwExecuteClassInitializeMethodsAfterLoadNotification >> defaultAction [

	^ false

]
