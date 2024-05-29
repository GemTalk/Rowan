Class {
	#name : 'RwAuditTraitPropertyDetail',
	#superclass : 'RwAuditDetail',
	#instVars : [
		'trait',
		'loadedPropertyValue',
		'traitPropertyValue'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'accessing' }
RwAuditTraitPropertyDetail >> loadedPropertyValue [
	^loadedPropertyValue
]

{ #category : 'accessing' }
RwAuditTraitPropertyDetail >> loadedPropertyValue: object [
	loadedPropertyValue := object
]

{ #category : 'accessing' }
RwAuditTraitPropertyDetail >> trait [
	^trait
]

{ #category : 'accessing' }
RwAuditTraitPropertyDetail >> trait: object [
	trait := object
]

{ #category : 'accessing' }
RwAuditTraitPropertyDetail >> traitPropertyValue [
	^traitPropertyValue
]

{ #category : 'accessing' }
RwAuditTraitPropertyDetail >> traitPropertyValue: object [
	traitPropertyValue := object
]
