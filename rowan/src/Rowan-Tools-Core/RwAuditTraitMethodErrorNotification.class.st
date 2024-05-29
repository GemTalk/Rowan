Class {
	#name : 'RwAuditTraitMethodErrorNotification',
	#superclass : 'RwAuditTraitErrorNotification',
	#instVars : [
		'method',
		'selector',
		'isMetaclass'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'creation' }
RwAuditTraitMethodErrorNotification class >> method: methodSelector isMeta: isMeta inTraitNamed: traitName intoPackageNamed: packageName [
	^ (self traitNamed: traitName intoPackageNamed: packageName)
		selector: methodSelector;
		isMetaclass: isMeta;
		yourself
]

{ #category : 'private' }
RwAuditTraitMethodErrorNotification >> _errorMessage [
	^ 'The trait named ' , self traitName , ' is missing it''s loaded method for '
		, self methodPrintString
]

{ #category : 'accessing' }
RwAuditTraitMethodErrorNotification >> isMetaclass [
	^isMetaclass
]

{ #category : 'accessing' }
RwAuditTraitMethodErrorNotification >> isMetaclass: object [
	isMetaclass := object
]

{ #category : 'Handling' }
RwAuditTraitMethodErrorNotification >> methodErrorDo: methodBlock traitErrorDo: traitBlock [
	"helper method for use in exception handling block ... avoid isKindOf:"

	methodBlock value
]

{ #category : 'accessing' }
RwAuditTraitMethodErrorNotification >> methodPrintString [

	^ self 
		traitName, 
		(self isMetaclass ifTrue: [ ' classTrait >> ' ] ifFalse: [ ' >> ' ]),
		self selector
]

{ #category : 'accessing' }
RwAuditTraitMethodErrorNotification >> selector [
	^selector
]

{ #category : 'accessing' }
RwAuditTraitMethodErrorNotification >> selector: object [
	selector := object
]
