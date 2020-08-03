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

{ #category : 'instance creation' }
RowanProcessService class >> new [

	"in general use one of the other constructor methods.
	Needed for STON replication"
	^super new
]

{ #category : 'instance creation' }
RowanProcessService class >> onActiveProcess: aGsProcess [

	^self basicNew
		initialize: aGsProcess status: 'active';
		yourself

]

{ #category : 'instance creation' }
RowanProcessService class >> onReadyProcess: aGsProcess [

	^self basicNew
		initialize: aGsProcess status: 'ready';
		yourself

]

{ #category : 'instance creation' }
RowanProcessService class >> onSuspendedProcess: aGsProcess [

	^self basicNew
		initialize: aGsProcess status: 'suspended';
		yourself

]

{ #category : 'instance creation' }
RowanProcessService class >> onWaitingProcess: aGsProcess [

	^self basicNew
		initialize: aGsProcess status: 'waiting';
		yourself

]

{ #category : 'initialize' }
RowanProcessService >> initialize [

	frames := Array new
]

{ #category : 'initialize' }
RowanProcessService >> initialize: aGsProcess status: aString [

	| theOrganizer |
	theOrganizer := ClassOrganizer new. 
	frames := Array new: aGsProcess stackDepth.
	1 to: aGsProcess stackDepth do: [:i | 
		frames at: i put: (RowanFrameService process: aGsProcess level: i organizer: theOrganizer).
	].
	oop := aGsProcess asOop.  
	status := aString.
]

{ #category : 'accessing' }
RowanProcessService >> oop [
	^oop
]

{ #category : 'accessing' }
RowanProcessService >> oop: object [
	oop := object
]

{ #category : 'perform' }
RowanProcessService >> servicePerform: symbol withArguments: collection [
	^self perform: symbol withArguments: collection.
]

{ #category : 'accessing' }
RowanProcessService >> status [
	^status
]

{ #category : 'accessing' }
RowanProcessService >> status: object [
	status := object
]

{ #category : 'updating' }
RowanProcessService >> update [

	self initialize: (Object _objectForOop: oop) status: 'suspended'.
	RowanCommandResult addResult: self.
]
