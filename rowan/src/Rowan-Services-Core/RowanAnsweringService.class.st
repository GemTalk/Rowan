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

{ #category : 'other' }
RowanAnsweringService >> answer [

	^answer
]

{ #category : 'Updating' }
RowanAnsweringService >> answer: anObject [

	answer := anObject
]

{ #category : 'commands' }
RowanAnsweringService >> exec: aString [

	"for command line service someday"
	answer := aString evaluate printString. 
	RowanCommandResult addResult: self.
]

{ #category : 'commands' }
RowanAnsweringService >> flipTranscript [
	self isTranscriptInstalled ifTrue:[
		self jadeiteServer uninstallTranscript]
	ifFalse:[
		self jadeiteServer installTranscript]
]

{ #category : 'testing' }
RowanAnsweringService >> isTranscriptInstalled [

	^self transcriptObject == self jadeiteServer
]

{ #category : 'commands' }
RowanAnsweringService >> runMethodTests: methodServices [

	| behavior |
	methodServices do:[:methodService |
		(methodService selector asString matchPattern: #('test' $*)) ifTrue:[ 
			behavior := methodService classFromName. 
			behavior debug: methodService selector]].
	answer := true. 
	RowanCommandResult initializeResults. "squash any client updates during server test run"
	RowanCommandResult addResult: self
]

{ #category : 'commands' }
RowanAnsweringService >> selectorsMatchingPattern: pattern [

	answer := ((AllUsers userWithId: #SymbolUser) resolveSymbol: #AllSymbols) value.
	answer := answer select: [:each |each _matchPatternNoCase: pattern].
	answer := answer asSortedCollection asArray.
	command := nil. 
	commandArgs := nil. 
	RowanCommandResult addResult: self.
]

{ #category : 'commands' }
RowanAnsweringService >> subclassCreationTemplate: className [
	 (RowanClassService new name: className) subclassCreationTemplate.  "gives an answer for us"
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

{ #category : 'commands' }
RowanAnsweringService >> turnOffTranscriptWrites [

	self isTranscriptInstalled ifTrue:[
		self flipTranscript]
]
