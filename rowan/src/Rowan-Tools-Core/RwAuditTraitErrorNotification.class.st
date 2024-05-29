Class {
	#name : 'RwAuditTraitErrorNotification',
	#superclass : 'RwNotification',
	#instVars : [
		'traitName',
		'packageName',
		'description',
		'reason'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'creation' }
RwAuditTraitErrorNotification class >> traitNamed: traitName intoPackageNamed: packageName [
	^ self new
		traitName: traitName;
		packageName: packageName;
		yourself
]

{ #category : 'private' }
RwAuditTraitErrorNotification >> _errorMessage [

	self subclassResponsibility: #_errorMessage
]

{ #category : 'Handling' }
RwAuditTraitErrorNotification >> defaultAction [
 
	^ Error signal: self _errorMessage
]

{ #category : 'accessing' }
RwAuditTraitErrorNotification >> description [

	^ description ifNil: [ '' ]
]

{ #category : 'accessing' }
RwAuditTraitErrorNotification >> description: aString [

	description := aString
]

{ #category : 'Handling' }
RwAuditTraitErrorNotification >> methodErrorDo: methodBlock traitErrorDo: traitBlock [
	"helper method for use in exception handling block ... avoid isKindOf:"

	self subclassResponsibility: #methodErrorDo:traitErrorDo:
]

{ #category : 'accessing' }
RwAuditTraitErrorNotification >> packageName [
	^packageName
]

{ #category : 'accessing' }
RwAuditTraitErrorNotification >> packageName: object [
	packageName := object
]

{ #category : 'accessing' }
RwAuditTraitErrorNotification >> reason [
	^reason
]

{ #category : 'accessing' }
RwAuditTraitErrorNotification >> reason: object [
	reason := object
]

{ #category : 'accessing' }
RwAuditTraitErrorNotification >> traitName [
	^traitName
]

{ #category : 'accessing' }
RwAuditTraitErrorNotification >> traitName: object [
	traitName := object
]
