Class {
	#name : 'RwAdoptClassErrorNotification',
	#superclass : 'RwAdoptAuditErrorNotification',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwAdoptClassErrorNotification >> _errorMessage [

	| extensionMessage |
	extensionMessage := self isClassExtension
		ifTrue: [ ' extension ' ]
		ifFalse: [ ' ' ].
	^ 'Unable to adopt the class ', self className printString, ' into the', extensionMessage, 'package ', self packageName printString, ' as it is not present in the current user''s symbol list.'
]
