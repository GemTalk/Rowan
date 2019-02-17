"
All Cypress classes are private to GemStone and are likely to be removed in a future release.

CypressLoaderError is used to report a failure applying a specific CypressPatchOperation.
The CypressLoader made a first attempt to apply the Patch Operation and reported a 
CypressLoaderErrorNotification, set aside the Patch Operation, and has retried it after applying
all other Patch Operations.


Instance Variables:

patchOperation:		the CypressPatchOperation that could not be applied.
exception:			the Error which occurred while trying to apply the Patch Operation.

"
Class {
	#name : 'CypressLoaderError',
	#superclass : 'Error',
	#type : 'variable',
	#instVars : [
		'exception',
		'patchOperation'
	],
	#category : 'Cypress-Definitions'
}

{ #category : 'instance creation' }
CypressLoaderError class >> patchOperation: aPatchOperation exception: anException [

	^self new
		initializePatchOperation: aPatchOperation exception: anException;
		yourself

]

{ #category : 'accessing' }
CypressLoaderError >> exception [
	"Answer the original exception raised when applying the Patch Operation."

	^exception

]

{ #category : 'updating' }
CypressLoaderError >> exception: anException [
	"Assign the original exception raised when applying the Patch Operation."

	exception := anException

]

{ #category : 'initializing - private' }
CypressLoaderError >> initialize [

	super initialize.
	gsResumable := true

]

{ #category : 'initializing - private' }
CypressLoaderError >> initializeMessageText [

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	stream
		nextPutAll: self patchOperation printString;
		nextPutAll: ' failed because ';
		nextPutAll: self exception printString.
	messageText := stream contents

]

{ #category : 'initializing - private' }
CypressLoaderError >> initializePatchOperation: aPatchOperation exception: anException [

	self
		patchOperation: aPatchOperation;
		exception: anException;
		initializeMessageText

]

{ #category : 'handling' }
CypressLoaderError >> logNotification: aString [

	GsFile gciLogServer: aString.
	Transcript cr; nextPutAll: aString.

]

{ #category : 'accessing' }
CypressLoaderError >> patchOperation [
	"Answer the Patch Operation that could not be applied."

	^patchOperation

]

{ #category : 'updating' }
CypressLoaderError >> patchOperation: aCypressPatchOperation [
	"Assign the Patch Operation that could not be applied."

	patchOperation := aCypressPatchOperation

]
