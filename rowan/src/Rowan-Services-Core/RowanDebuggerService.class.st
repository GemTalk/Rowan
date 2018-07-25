Class {
	#name : 'RowanDebuggerService',
	#superclass : 'RowanService',
	#instVars : [
		'initialProcessOop',
		'processes'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'other' }
RowanDebuggerService >> update [

	processes := OrderedCollection with: (RowanProcessService onActiveProcess: (Object _objectForOop: initialProcessOop)).
	ProcessorScheduler scheduler readyProcesses 			do: [:each | processes add: (RowanProcessService onReadyProcess: 		each)].
	ProcessorScheduler scheduler suspendedProcesses 	do: [:each | processes add: (RowanProcessService onSuspendedProcess: 	each)].
	ProcessorScheduler scheduler waitingProcesses 		do: [:each | processes add: (RowanProcessService onWaitingProcess: 		each)].
	RowanCommandResult addResult: self.

]
