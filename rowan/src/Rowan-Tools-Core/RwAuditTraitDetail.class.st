Class {
	#name : 'RwAuditTraitDetail',
	#superclass : 'RwAuditDetail',
	#instVars : [
		'trait'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'accessing' }
RwAuditTraitDetail >> loadedTrait [
	^ self owner
]

{ #category : 'accessing' }
RwAuditTraitDetail >> trait [
	^trait
]

{ #category : 'accessing' }
RwAuditTraitDetail >> trait: object [
	trait := object
]
