"
Audit failure detail for class properties. 

Reasons:
	#differentSuperclass
	#differentClassInstVars
	#differentClassVars
	#differentPoolDictionaries
	#differentComment
	#differentCategory
	#missingSymbolDictionary
	#missingClassInSymbolDictionary
"
Class {
	#name : 'RwAuditClassPropertyDetail',
	#superclass : 'RwAuditDetail',
	#instVars : [
		'class',
		'loadedPropertyValue',
		'classPropertyValue'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'accessing' }
RwAuditClassPropertyDetail >> class [
	^class
]

{ #category : 'accessing' }
RwAuditClassPropertyDetail >> class: object [
	class := object
]

{ #category : 'accessing' }
RwAuditClassPropertyDetail >> classPropertyValue [
	^classPropertyValue
]

{ #category : 'accessing' }
RwAuditClassPropertyDetail >> classPropertyValue: object [
	classPropertyValue := object
]

{ #category : 'accessing' }
RwAuditClassPropertyDetail >> loadedClass [
	^ self owner
]

{ #category : 'accessing' }
RwAuditClassPropertyDetail >> loadedPropertyValue [
	^loadedPropertyValue
]

{ #category : 'accessing' }
RwAuditClassPropertyDetail >> loadedPropertyValue: object [
	loadedPropertyValue := object
]
