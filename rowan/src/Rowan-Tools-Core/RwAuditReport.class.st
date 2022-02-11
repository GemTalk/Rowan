Class {
	#name : 'RwAuditReport',
	#superclass : 'StringKeyValueDictionary',
	#instVars : [
		'owner'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'other' }
RwAuditReport class >> for: anObject [

	^self new owner: anObject; 
			log;
			yourself

]

{ #category : 'other' }
RwAuditReport >> log [

	GsFile gciLogClient: '==== Auditing ', owner prettyName.
]

{ #category : 'other' }
RwAuditReport >> owner [

	^owner

]

{ #category : 'other' }
RwAuditReport >> owner: anObject [


	owner := anObject

]

{ #category : 'other' }
RwAuditReport >> printOn: aStream [
	
	aStream nextPutAll: '==============Auditing ', self owner prettyName asString; lf.
	self do: [:e | 
		(e isKindOf: Array) ifTrue: [
			e do: [:err | err printOn: aStream. aStream lf]
		] ifFalse: [e printOn: aStream].
	]

]
