Class {
	#name : 'RwAdoptClassErrorNotification',
	#superclass : 'RwAdoptErrorNotification',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'instance creation' }
RwAdoptClassErrorNotification class >> failureAdoptingClassNamed: className isClassExtension: classExtension  intoPackageNamed: packageName [

	^ self new
		className: className;
		isClassExtension: classExtension;
		packageName: packageName;
		yourself
]

{ #category : 'private' }
RwAdoptClassErrorNotification >> _errorMessage [

	| extensionMessage |
	extensionMessage := self isClassExtension
		ifTrue: [ ' extension ' ]
		ifFalse: [ ' ' ].
	^ 'Unable to adopt the class ', self className printString, ' into the', extensionMessage, 'package ', self packageName printString, ' as it is not present in the current user''s symbol list.'
]
