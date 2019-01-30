"
A place to put miscellaneous commands that don't fit 
well in other services. 

Also good asking the server questions as it gives back 
an answer whereas other services simply return updated
services.
"
Class {
	#name : 'RowanAnsweringService',
	#superclass : 'RowanService',
	#instVars : [
		'answer'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'client commands' }
RowanAnsweringService >> allTestsIn: classServices [
	answer := Array new. 
	classServices do:[:service | answer addAll: service allTests].
	RowanCommandResult addResult: self.
]

{ #category : 'other' }
RowanAnsweringService >> answer [

	^answer
]

{ #category : 'Updating' }
RowanAnsweringService >> answer: anObject [

	answer := anObject
]

{ #category : 'client commands' }
RowanAnsweringService >> exec: aString [

	"for command line service someday"
	answer := aString evaluate printString. 
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanAnsweringService >> flipAutoCommit [

	answer := RowanService flipAutoCommit. 
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanAnsweringService >> flipTranscript [
	self isTranscriptInstalled ifTrue:[
		self jadeiteServer uninstallTranscript]
	ifFalse:[
		self jadeiteServer installTranscript]
]

{ #category : 'client commands' }
RowanAnsweringService >> initializeAutoCommit [

	RowanService setAutoCommit: false
]

{ #category : 'testing' }
RowanAnsweringService >> isTranscriptInstalled [

	^self transcriptObject == self jadeiteServer
]

{ #category : 'client commands' }
RowanAnsweringService >> loadedPackageExists: packageName [
	
	| actualName |
	actualName := Rowan image packageNames detect:[:loadedName | loadedName asLowercase = packageName asLowercase] ifNone:[]. 
	answer := (Rowan image loadedPackageNamed: actualName ifAbsent: []) notNil. 
	command := nil. 
	commandArgs := nil. 
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
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

{ #category : 'client commands' }
RowanAnsweringService >> selectorsMatchingPattern: pattern [

	answer := ((AllUsers userWithId: #SymbolUser) resolveSymbol: #AllSymbols) value.
	answer := answer select: [:each |each _matchPatternNoCase: pattern].
	answer := answer asSortedCollection asArray.
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanAnsweringService >> setAutoCommit: boolean [

	answer := RowanService setAutoCommit: boolean.
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanAnsweringService >> subclassCreationTemplate: className [
	 (RowanClassService new name: className) subclassCreationTemplate.  "gives an answer for us"
]

{ #category : 'client commands' }
RowanAnsweringService >> transcriptInstalled [

	answer := self isTranscriptInstalled.
	RowanCommandResult addResult: self.
]

{ #category : 'private' }
RowanAnsweringService >> transcriptObject [
	
	^(SessionTemps current  at: #'TranscriptStream_SessionStream')
]

{ #category : 'client commands' }
RowanAnsweringService >> turnOffTranscriptWrites [

	self isTranscriptInstalled ifTrue:[
		self flipTranscript]
]
