Class {
	#name : 'RwAdoptAuditErrorNotification',
	#superclass : 'RwNotification',
	#instVars : [
		'className',
		'isClassExtension',
		'packageName',
		'description',
		'reason'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'instance creation' }
RwAdoptAuditErrorNotification class >> classNamed: className isClassExtension: classExtension  intoPackageNamed: packageName [

	^ self new
		className: className;
		isClassExtension: classExtension;
		packageName: packageName;
		yourself
]

{ #category : 'private' }
RwAdoptAuditErrorNotification >> _errorMessage [

	self subclassResponsibility: #_errorMessage
]

{ #category : 'accessing' }
RwAdoptAuditErrorNotification >> className [

	^ className
]

{ #category : 'accessing' }
RwAdoptAuditErrorNotification >> className: aString [

	className := aString
]

{ #category : 'Handling' }
RwAdoptAuditErrorNotification >> defaultAction [
 
	^ Error signal: self _errorMessage
]

{ #category : 'accessing' }
RwAdoptAuditErrorNotification >> description [

	^ description ifNil: [ '' ]
]

{ #category : 'accessing' }
RwAdoptAuditErrorNotification >> description: aString [

	description := aString
]

{ #category : 'accessing' }
RwAdoptAuditErrorNotification >> isClassExtension [

	^ isClassExtension
]

{ #category : 'accessing' }
RwAdoptAuditErrorNotification >> isClassExtension: aBoolean [

	isClassExtension := aBoolean
]

{ #category : 'Handling' }
RwAdoptAuditErrorNotification >> methodErrorDo: methodBlock classErrorDo: classBlock [
	"helper method for use in exception handling block ... avoid isKindOf:"

	self subclassResponsibility: #methodErrorDo:classErrorDo:
]

{ #category : 'accessing' }
RwAdoptAuditErrorNotification >> packageName [

	^ packageName
]

{ #category : 'accessing' }
RwAdoptAuditErrorNotification >> packageName: aString [

	packageName := aString
]

{ #category : 'accessing' }
RwAdoptAuditErrorNotification >> reason [
	^reason
]

{ #category : 'accessing' }
RwAdoptAuditErrorNotification >> reason: object [
	reason := object
]
