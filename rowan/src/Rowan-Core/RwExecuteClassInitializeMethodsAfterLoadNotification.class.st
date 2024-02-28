Class {
	#name : 'RwExecuteClassInitializeMethodsAfterLoadNotification',
	#superclass : 'RwNotification',
	#instVars : [
		'candidateClass'
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
	"perform class initialization"

	^ true
]
