"
Audit failure detail for a class. 

Reasons:
	#missingGemStoneClassForLoadedClass
	#missingGemStoneClassForLoadedClassExtension
	#emptyLoadedClassExtension
	#missingCompiledMethodsForLoadedClassExtension
"
Class {
	#name : 'RwAuditClassDetail',
	#superclass : 'RwAuditDetail',
	#instVars : [
		'behavior'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'accessing' }
RwAuditClassDetail >> behavior [
	^behavior
]

{ #category : 'accessing' }
RwAuditClassDetail >> behavior: object [
	behavior := object
]

{ #category : 'accessing' }
RwAuditClassDetail >> loadedClass [
	^ self owner
]
