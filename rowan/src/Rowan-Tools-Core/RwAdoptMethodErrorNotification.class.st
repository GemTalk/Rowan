Class {
	#name : 'RwAdoptMethodErrorNotification',
	#superclass : 'RwAdoptErrorNotification',
	#instVars : [
		'selector',
		'isMetaclass'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'instance creation' }
RwAdoptMethodErrorNotification class >> failureAdoptingMethod: methodSelector isMeta: isMeta inClassNamed: className isClassExtension: classExtension  intoPackageNamed: packageName [


	^ self new
		selector: methodSelector;
		isMetaclass: isMeta;
		className: className;
		isClassExtension: classExtension;
		packageName: packageName;
		yourself
]

{ #category : 'private' }
RwAdoptMethodErrorNotification >> _errorMessage [

	| extensionMessage metaClassMessage |
	metaClassMessage := self isMetaclass
		ifTrue: [ ' class << #' ]
		ifFalse: [ '  << #' ].
	extensionMessage := self isClassExtension
		ifTrue: [ ' extension ' ]
		ifFalse: [ ' ' ].
	^ 'Unable to adopt the method ', self className, metaClassMessage, self selector asString, ' into the package ', self packageName printString, '. The method is not present in the class''s method dictionary.'
]

{ #category : 'accessing' }
RwAdoptMethodErrorNotification >> isMetaclass [

	^ isMetaclass
]

{ #category : 'accessing' }
RwAdoptMethodErrorNotification >> isMetaclass: aBoolean [

	isMetaclass := aBoolean
]

{ #category : 'accessing' }
RwAdoptMethodErrorNotification >> selector [

	^ selector
]

{ #category : 'accessing' }
RwAdoptMethodErrorNotification >> selector: aString [

	selector := aString
]
