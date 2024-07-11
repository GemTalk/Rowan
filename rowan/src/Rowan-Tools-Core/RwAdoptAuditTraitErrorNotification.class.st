Class {
	#name : 'RwAdoptAuditTraitErrorNotification',
	#superclass : 'RwAdoptAuditErrorNotification',
	#instVars : [
		'traitName'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'instance creation' }
RwAdoptAuditTraitErrorNotification class >> traitNamed: traitName intoPackageNamed: packageName [

	^ self new
		traitName: traitName;
		packageName: packageName;
		yourself
]

{ #category : 'Handling' }
RwAdoptAuditTraitErrorNotification >> methodErrorDo: methodBlock classErrorDo: classBlock traitErrorDo: traitBlock [
	"helper method for use in exception handling block ... avoid isKindOf:"

	traitBlock value
]

{ #category : 'accessing' }
RwAdoptAuditTraitErrorNotification >> traitName [
	^traitName
]

{ #category : 'accessing' }
RwAdoptAuditTraitErrorNotification >> traitName: object [
	traitName := object
]
