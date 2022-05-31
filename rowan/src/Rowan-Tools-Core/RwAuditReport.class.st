Class {
	#name : 'RwAuditReport',
	#superclass : 'StringKeyValueDictionary',
	#instVars : [
		'owner',
		'logStream'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'other' }
RwAuditReport class >> for: anObject [

	^self new owner: anObject on: nil
]

{ #category : 'other' }
RwAuditReport class >> for: anObject on: logStreamOrNil [
	^ self new
		logStream: logStreamOrNil;
		owner: anObject;
		log;
		yourself
]

{ #category : 'other' }
RwAuditReport class >> on: logStreamOrNil [
	^ self new
		logStream: logStreamOrNil;
		yourself
]

{ #category : 'logging' }
RwAuditReport >> log [
	owner ifNotNil: [ self logMessage: '==== Auditing ' , owner prettyName ]
]

{ #category : 'logging' }
RwAuditReport >> logMessage: aMessage [
	logStream
		ifNil: [ GsFile gciLogClient: aMessage ]
		ifNotNil: [ 
			logStream
				nextPutAll: aMessage;
				lf ]
]

{ #category : 'accessing' }
RwAuditReport >> logStream [
	^logStream
]

{ #category : 'accessing' }
RwAuditReport >> logStream: object [
	logStream := object
]

{ #category : 'accessing' }
RwAuditReport >> owner [

	^owner

]

{ #category : 'accessing' }
RwAuditReport >> owner: anObject [


	owner := anObject

]

{ #category : 'printing' }
RwAuditReport >> printOn: aStream [
	self owner
		ifNotNil: [ 
			aStream
				nextPutAll: '==============Auditing ' , self owner prettyName asString;
				lf ].
	self
		do: [ :e | 
			(e isKindOf: Array)
				ifTrue: [ 
					e
						do: [ :err | 
							err printOn: aStream.
							aStream lf ] ]
				ifFalse: [ e printOn: aStream ] ]
]
