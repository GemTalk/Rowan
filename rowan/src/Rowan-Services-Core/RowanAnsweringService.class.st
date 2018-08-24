"
A place to put miscellaneous commands that don't fit 
well in other services. 

Also good asking the server questions.
"
Class {
	#name : 'RowanAnsweringService',
	#superclass : 'RowanService',
	#instVars : [
		'answer'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'Updating' }
RowanAnsweringService >> answer: anObject [

	answer := anObject
]

{ #category : 'commands' }
RowanAnsweringService >> flipTranscript [
	self isTranscriptInstalled ifTrue:[
		self jadeServer uninstallTranscript]
	ifFalse:[
		self jadeServer installTranscript]
]

{ #category : 'testing' }
RowanAnsweringService >> isTranscriptInstalled [

	^self transcriptObject == self jadeServer
]

{ #category : 'private' }
RowanAnsweringService >> jadeServer [

	^System _sessionStateAt: 3
]

{ #category : 'commands' }
RowanAnsweringService >> transcriptInstalled [

	answer := self isTranscriptInstalled.
	RowanCommandResult addResult: self.
]

{ #category : 'private' }
RowanAnsweringService >> transcriptObject [
	
	^(SessionTemps current  at: #'TranscriptStream_SessionStream')
]
