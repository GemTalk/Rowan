Class {
	#name : 'RwAdoptAuditClassErrorNotification',
	#superclass : 'RwAdoptAuditErrorNotification',
	#instVars : [
		'className',
		'isClassExtension'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'instance creation' }
RwAdoptAuditClassErrorNotification class >> classNamed: className isClassExtension: classExtension  intoPackageNamed: packageName [

	^ self new
		className: className;
		isClassExtension: classExtension;
		packageName: packageName;
		yourself
]

{ #category : 'accessing' }
RwAdoptAuditClassErrorNotification >> className [
	^className
]

{ #category : 'accessing' }
RwAdoptAuditClassErrorNotification >> className: object [
	className := object
]

{ #category : 'accessing' }
RwAdoptAuditClassErrorNotification >> isClassExtension [
	^isClassExtension
]

{ #category : 'accessing' }
RwAdoptAuditClassErrorNotification >> isClassExtension: object [
	isClassExtension := object
]

{ #category : 'Handling' }
RwAdoptAuditClassErrorNotification >> methodErrorDo: methodBlock classErrorDo: classBlock traitErrorDo: traitBlock [
	"helper method for use in exception handling block ... avoid isKindOf:"

	classBlock value
]
