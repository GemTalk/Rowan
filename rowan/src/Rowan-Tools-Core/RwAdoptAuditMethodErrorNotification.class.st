Class {
	#name : 'RwAdoptAuditMethodErrorNotification',
	#superclass : 'RwAdoptAuditErrorNotification',
	#instVars : [
		'selector',
		'isMetaclass',
		'isTrait',
		'className',
		'isClassExtension'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'instance creation' }
RwAdoptAuditMethodErrorNotification class >> method: methodSelector isMeta: isMeta inClassNamed: className isClassExtension: classExtension intoPackageNamed: packageName [
	^ (self
		classNamed: className
		isClassExtension: classExtension
		intoPackageNamed: packageName)
		selector: methodSelector;
		isMetaclass: isMeta;
		yourself
]

{ #category : 'instance creation' }
RwAdoptAuditMethodErrorNotification class >> method: methodSelector isMeta: isMeta inTraitNamed: traitName intoPackageNamed: packageName [
	^ (self
		classNamed: traitName
		isClassExtension: false
		intoPackageNamed: packageName)
		isTrait: true;
		selector: methodSelector;
		isMetaclass: isMeta;
		yourself
]

{ #category : 'accessing' }
RwAdoptAuditMethodErrorNotification >> className [
	^className
]

{ #category : 'accessing' }
RwAdoptAuditMethodErrorNotification >> className: object [
	className := object
]

{ #category : 'accessing' }
RwAdoptAuditMethodErrorNotification >> isClassExtension [
	^isClassExtension
]

{ #category : 'accessing' }
RwAdoptAuditMethodErrorNotification >> isClassExtension: object [
	isClassExtension := object
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
RwAdoptAuditMethodErrorNotification >> isTrait [
	^ isTrait ifNil: [ ^ false ]
]

{ #category : 'accessing' }
RwAdoptAuditMethodErrorNotification >> isTrait: object [
	isTrait := object
]

{ #category : 'Handling' }
RwAdoptAuditMethodErrorNotification >> methodErrorDo: methodBlock classErrorDo: classBlock traitErrorDo: traitBlock [
	"helper method for use in exception handling block ... avoid isKindOf:"

	self isTrait
		ifTrue: [ traitBlock value ]
		ifFalse: [ methodBlock value ]
]

{ #category : 'accessing' }
RwAdoptAuditMethodErrorNotification >> methodPrintString [

	^ self 
		className, 
		(self isMetaclass ifTrue: [ ' class >> ' ] ifFalse: [ ' >> ' ]),
		self selector
]

{ #category : 'accessing' }
RwAdoptAuditMethodErrorNotification >> selector [

	^ selector
]

{ #category : 'accessing' }
RwAdoptAuditMethodErrorNotification >> selector: aString [

	selector := aString
]
