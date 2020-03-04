Class {
	#name : 'RowanLoggingService',
	#superclass : 'RowanService',
	#instVars : [
		'fileName',
		'id',
		'groupId',
		'date',
		'time',
		'comment',
		'services',
		'mode',
		'location',
		'isLogging'
	],
	#classVars : [
		'Current'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanLoggingService class >> current [

	"lazy initialize for a topaz session test" 
	^SessionTemps current at: #rowanLoggingService ifAbsentPut: [RowanLoggingService new]
]

{ #category : 'accessing' }
RowanLoggingService class >> current: anObject [

	SessionTemps current at: #rowanLoggingService put: anObject
]

{ #category : 'accessing' }
RowanLoggingService >> fileName [
	^fileName
]

{ #category : 'accessing' }
RowanLoggingService >> fileName: object [
	fileName := object
]

{ #category : 'initialization' }
RowanLoggingService >> initialize [

	super initialize. 
	isLogging := false.
]

{ #category : 'client commands' }
RowanLoggingService >> logComment: string [

	| stonString ws |
	isLogging ifFalse:[^self].
	comment := string.
	id := id + 1. 
	date := Date today.
	time := Time now.
	location := #server.
	stonString := STON toString: self.
	ws := FileStreamPortable 
				write: fileName
				mode: #append.
	[ws nextPutAll: stonString] ensure: [ws close].
	comment := nil "service may be reused. Clear comment"
]

{ #category : 'accessing' }
RowanLoggingService >> logFileContents [

	| rs |
	rs := [FileStreamPortable read: fileName] on: Error do:[:ex | ^String new].
	[^rs contents] ensure: [rs close]
]

{ #category : 'client commands' }
RowanLoggingService >> logReceivedServices [

	mode := #received.
	self class current: self. 
	self logServices
]

{ #category : 'client commands' }
RowanLoggingService >> logSentServices [

	mode := #sent.
	services := RowanCommandResult results copy asOrderedCollection.
	self logServices.
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanLoggingService >> logServices [

	| stonString ws |
	isLogging ifFalse:[^self].
	id := id + 1. 
	date := Date today.
	time := Time now.
	location := #server.
	stonString := STON toString: self.
	ws := FileStreamPortable 
				write: fileName
				mode: #append.
	[ws nextPutAll: stonString] ensure: [ws close]
]
