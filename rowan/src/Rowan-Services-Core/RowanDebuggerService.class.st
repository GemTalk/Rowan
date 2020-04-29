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
  | debugStream newStream char peekChar |
  debugStream := ReadStream on: aString trimLeadingBlanks.
  [ 
  peekChar := debugStream peek.
  peekChar = Character tab
    or: [ peekChar = Character cr or: [ peekChar = Character lf ] ]	"trimLeadingBlanks doesn't look for tab or cr or lf" ]
    whileTrue: [ debugStream next ].
  debugStream contents isEmpty
    ifTrue: [ ^ 'nil halt.' ].
  newStream := WriteStream on: String new.
  (char := debugStream next) = $|
    ifTrue: [ 
      newStream nextPut: char.
      newStream
        nextPutAll: (debugStream upTo: $|);
        nextPut: $|;
        nextPut: Character space;
        nextPutAll: 'nil halt. ' ]
    ifFalse: [ 
      newStream
        nextPutAll: 'nil halt. ';
        nextPut: char ].
  newStream nextPutAll: debugStream upToEnd.
  ^ newStream contents
]

{ #category : 'release' }
RowanDebuggerService >> releaseProcessOop: oop [
  "not really releasing it. The client should have registered
	the process with the debugger window it opened before
	this is run"

  | jadeiteProcesses process |
  ((process := Object _objectForOop: oop) isKindOf: GsProcess)
    ifTrue: [ 
      jadeiteProcesses := SessionTemps current
        at: #'jadeiteProcesses'
        ifAbsentPut: [ Array new ].
      jadeiteProcesses remove: process ifAbsent: [  ] ]
]

{ #category : 'release' }
RowanDebuggerService >> saveProcessOop: processOop [
  | jadeiteProcesses process |
  process := Object _objectForOop: processOop.
  (process isKindOf: GsProcess)
    ifTrue: [ 
      jadeiteProcesses := SessionTemps current
        at: #'jadeiteProcesses'
        ifAbsentPut: [ Array new ].
      (jadeiteProcesses includes: process)
        ifFalse: [ 
          "tests may pass through this method twice"
          jadeiteProcesses add: process ] ]
]

{ #category : 'perform' }
RowanDebuggerService >> servicePerform: symbol withArguments: collection [
	^self perform: symbol withArguments: collection.
]

{ #category : 'clientCommands' }
RowanDebuggerService >> terminateProcess: processOop [
  | process |
  process := Object _objectForOop: processOop.
  (process isKindOf: GsProcess)
    ifFalse: [ ^ self ].
  RowanDebuggerService new releaseProcessOop: processOop.
  process terminate.
  (Delay forMilliseconds: 10) wait	"allow forked processes to finish"
]

{ #category : 'updating' }
RowanDebuggerService >> update [
  processes := OrderedCollection
    with:
      (RowanProcessService onActiveProcess: (Object _objectForOop: initialProcessOop)).
  ProcessorScheduler scheduler readyProcesses
    do: [ :each | processes add: (RowanProcessService new oop: each asOop; status: 'ready')]. 
  ProcessorScheduler scheduler suspendedProcesses
    do: [ :each | processes add: (RowanProcessService new oop: each asOop; status: 'suspended')]. 
  ProcessorScheduler scheduler waitingProcesses
    do: [ :each | processes add: (RowanProcessService new oop: each asOop; status: 'waiting')]. 
  RowanCommandResult addResult: self.
  self releaseProcessOop: initialProcessOop
]
