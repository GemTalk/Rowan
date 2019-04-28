Class {
	#name : 'RwAdoptAuditMethodErrorNotification',
	#superclass : 'RwAdoptAuditErrorNotification',
	#instVars : [
		'selector',
		'isMetaclass'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'instance creation' }
RwAdoptAuditMethodErrorNotification class >> method: methodSelector isMeta: isMeta inClassNamed: className isClassExtension: classExtension  intoPackageNamed: packageName [


	^ (self classNamed: className isClassExtension: classExtension  intoPackageNamed: packageName)
		selector: methodSelector;
		isMetaclass: isMeta;
		yourself
]

{ #category : 'accessing' }
RwAdoptAuditMethodErrorNotification >> isMetaclass [

	^ isMetaclass
]

{ #category : 'accessing' }
RwAdoptAuditMethodErrorNotification >> isMetaclass: aBoolean [

	isMetaclass := aBoolean
]

{ #category : 'accessing' }
RwAdoptAuditMethodErrorNotification >> selector [

	^ selector
]

{ #category : 'accessing' }
RwAdoptAuditMethodErrorNotification >> selector: aString [

	selector := aString
]
