"
All Cypress classes are private to GemStone and are likely to be removed in a future release.

CypressLoaderErrorNotification is used to notify a consumer of the CypressLoader that a particular CypressPatchOperation failed.
As a Notification, it resumes by default, logging the error to the Transcript.


Instance Variables:

patchOperation:		the CypressPatchOperation that could not be applied.
exception:			the Error which occurred while trying to apply the Patch Operation.

"
Class {
	#name : 'CypressLoaderErrorNotification',
	#superclass : 'Notification',
	#instVars : [
		'exception',
		'patchOperation'
	],
	#category : 'Cypress-Definitions'
}

{ #category : 'instance creation' }
CypressLoaderErrorNotification class >> patchOperation: aPatchOperation exception: anException [

	^self new
		initializePatchOperation: aPatchOperation exception: anException;
		yourself

]

{ #category : 'handling' }
CypressLoaderErrorNotification >> defaultAction [
	"Log the notification to the GCI log and the Transcript, then resume."

	self logNotification: 'Notice: ' , self asString.
	^super defaultAction

]

{ #category : 'accessing' }
CypressLoaderErrorNotification >> exception [
	"Answer the original exception raised when applying the Patch Operation."

	^exception

]

{ #category : 'updating' }
CypressLoaderErrorNotification >> exception: anException [
	"Assign the original exception raised when applying the Patch Operation."

	exception := anException

]

{ #category : 'initializing - private' }
CypressLoaderErrorNotification >> initializeMessageText [

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	stream
		nextPutAll: self patchOperation printString;
		nextPutAll: ' failed because ';
		nextPutAll: self exception printString.
	messageText := stream contents

]

{ #category : 'initializing - private' }
CypressLoaderErrorNotification >> initializePatchOperation: aPatchOperation exception: anException [

	self
		patchOperation: aPatchOperation;
		exception: anException;
		initializeMessageText

]

{ #category : 'handling' }
CypressLoaderErrorNotification >> logNotification: aString [

	GsFile gciLogServer: aString.
	Transcript cr; nextPutAll: aString.

]

{ #category : 'accessing' }
CypressLoaderErrorNotification >> patchOperation [
	"Answer the Patch Operation that could not be applied."

	^patchOperation

]

{ #category : 'updating' }
CypressLoaderErrorNotification >> patchOperation: aCypressPatchOperation [
	"Assign the Patch Operation that could not be applied."

	patchOperation := aCypressPatchOperation

]
