"
Audit failure detail for a class. 

Reasons:
	#loadedPackageNotInRegistry
	#loadedPackageInWrongRegistry
"
Class {
	#name : 'RwAuditDetail',
	#superclass : 'Object',
	#instVars : [
		'reason',
		'message',
		'owner'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'other' }
RwAuditDetail class >> for: anObject message: aString [	
"add error to results. print to file"
	
	^self new owner: anObject; 
			message: aString; 
			notify; 
			yourself

]

{ #category : 'other' }
RwAuditDetail class >> for: anObject reason: aSymbol message: aString [
	"add error to results. print to file"

	^ self new
		owner: anObject;
		reason: aSymbol;
		message: aString;
		notify;
		yourself
]

{ #category : 'other' }
RwAuditDetail >> matches: aString [
"see if my messages matches expected string"

	^self message matchPattern: {$* . aString . $*}
]

{ #category : 'other' }
RwAuditDetail >> message [

	^message

]

{ #category : 'other' }
RwAuditDetail >> message: aString [

	message := aString

]

{ #category : 'other' }
RwAuditDetail >> notify [

	GsFile gciLogServer: self message,'  ', self owner name asString.
	Notification signal: self message asString,'  ', self owner name asString.

]

{ #category : 'other' }
RwAuditDetail >> owner [
	^owner

]

{ #category : 'other' }
RwAuditDetail >> owner: anObject [

	owner := anObject

]

{ #category : 'other' }
RwAuditDetail >> printOn: aStream [

	aStream 
			nextPutAll: '---------------';
			nextPutAll: self owner name asString.
	self owner isMeta ifTrue: [aStream space; nextPutAll: 'class'].
	aStream
			nextPutAll: '>>';
			nextPutAll: self message

]

{ #category : 'accessing' }
RwAuditDetail >> reason [
	^reason
]

{ #category : 'accessing' }
RwAuditDetail >> reason: aSymbol [
	reason := aSymbol
]
