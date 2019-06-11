Class {
	#name : 'RowanDebuggerService',
	#superclass : 'RowanService',
	#instVars : [
		'initialProcessOop',
		'processes'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'debug string' }
RowanDebuggerService >> debugStringFrom: aString [
	| debugStream newStream char | 
	debugStream := ReadStream on: aString trimLeadingBlanks.
	debugStream contents isEmpty ifTrue:[^'nil halt.'].
	newStream := WriteStream on: String new. 
	((char := debugStream next) = $|) ifTrue:[
		newStream nextPut: char. 
		newStream nextPutAll: (debugStream upTo: $|); 
			nextPut: $|;
			nextPut: Character space;
			nextPutAll: 'nil halt. '
	]
	ifFalse: [
		newStream 
			nextPutAll: 'nil halt. ';
			nextPut: char]. 		
	newStream nextPutAll: debugStream upToEnd. 
	^newStream contents
]

{ #category : 'perform' }
RowanDebuggerService >> servicePerform: symbol withArguments: collection [
	^self perform: symbol withArguments: collection.
]

{ #category : 'other' }
RowanDebuggerService >> update [

	processes := OrderedCollection with: (RowanProcessService onActiveProcess: (Object _objectForOop: initialProcessOop)).
	ProcessorScheduler scheduler readyProcesses 			do: [:each | processes add: (RowanProcessService onReadyProcess: 		each)].
	ProcessorScheduler scheduler suspendedProcesses 	do: [:each | processes add: (RowanProcessService onSuspendedProcess: 	each)].
	ProcessorScheduler scheduler waitingProcesses 		do: [:each | processes add: (RowanProcessService onWaitingProcess: 		each)].
	RowanCommandResult addResult: self.

]
