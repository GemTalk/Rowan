"
No class-specific documentation for RowanDebuggerService, hierarchy is:
Object
  RowanService( definition updates command commandArgs updateType organizer)
    RowanDebuggerService( initialProcessOop processes)

"
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

{ #category : 'other' }
RowanDebuggerService >> registeredWindowsIncludesOop: oop [
	| dictionary registeredOops |
	dictionary := SessionTemps current
		at: #'rowanServicesWindowRegistry'
		ifAbsentPut: [ Dictionary new ].
	registeredOops := Array new.
	dictionary values
		do: [ :array | array do: [ :object | registeredOops add: object asOop ] ].
	^ registeredOops includes: oop
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
RowanDebuggerService >> updateProcesses [
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
  (self registeredWindowsIncludesOop: initialProcessOop) 
		ifTrue:[	"the oop should be there but just in case" self releaseProcessOop: initialProcessOop.].
]
