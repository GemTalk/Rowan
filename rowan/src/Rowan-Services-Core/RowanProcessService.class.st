Class {
	#name : 'RowanProcessService',
	#superclass : 'RowanService',
	#instVars : [
		'frames',
		'oop',
		'status'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'other' }
RowanProcessService class >> new [

	self error: 'Use on*Process: constructors'.

]

{ #category : 'other' }
RowanProcessService class >> onActiveProcess: aGsProcess [

	^self basicNew
		initialize: aGsProcess status: 'active';
		yourself

]

{ #category : 'other' }
RowanProcessService class >> onReadyProcess: aGsProcess [

	^self basicNew
		initialize: aGsProcess status: 'ready';
		yourself

]

{ #category : 'other' }
RowanProcessService class >> onSuspendedProcess: aGsProcess [

	^self basicNew
		initialize: aGsProcess status: 'suspended';
		yourself

]

{ #category : 'other' }
RowanProcessService class >> onWaitingProcess: aGsProcess [

	^self basicNew
		initialize: aGsProcess status: 'waiting';
		yourself

]

{ #category : 'other' }
RowanProcessService >> initialize: aGsProcess status: aString [

	| organizer |
	organizer := ClassOrganizer new.
	frames := Array new: aGsProcess stackDepth.
	1 to: aGsProcess stackDepth do: [:i | 
		frames at: i put: (RowanFrameService process: aGsProcess level: i organizer: organizer).
	].
	oop := aGsProcess asOop.
	status := aString.

]
