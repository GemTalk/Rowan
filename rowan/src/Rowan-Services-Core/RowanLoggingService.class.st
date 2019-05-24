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

	^Current
]

{ #category : 'accessing' }
RowanLoggingService class >> current: anObject [

	^Current := anObject
]

{ #category : 'accessing' }
RowanLoggingService >> fileName [
	^fileName
]

{ #category : 'accessing' }
RowanLoggingService >> fileName: object [
	fileName := object
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
	ws := FileStream 
				write: fileName
				mode: #append.
	[ws nextPutAll: stonString] ensure: [ws close].
	comment := nil "service may be reused. Clear comment"
]

{ #category : 'accessing' }
RowanLoggingService >> logFileContents [

	| rs |
	rs := [FileStream read: fileName] on: Error do:[:ex | ^String new].
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
	services := RowanCommandResult results. 
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
	ws := FileStream 
				write: fileName
				mode: #append.
	[ws nextPutAll: stonString] ensure: [ws close]
]
