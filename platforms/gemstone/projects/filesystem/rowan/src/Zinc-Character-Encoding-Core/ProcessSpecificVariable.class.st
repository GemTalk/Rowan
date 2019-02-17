Class {
	#name : 'ProcessSpecificVariable',
	#superclass : 'Object',
	#instVars : [
		'index'
	],
	#classInstVars : [
		'soleInstance'
	],
	#category : 'Zinc-Character-Encoding-Core'
}

{ #category : 'ps - session temps' }
ProcessSpecificVariable class >> allocatePSKey: aPSVariable [

	"Add a new process-specific key. 
	If an object already registered as a key, answer its index,
	if object is not registered, first search for an empty slot for insertion and if not found, grow an array to add new object"

	| anArray index |
	self psKeysSemaphore critical: [
		anArray := self psKeys.
		index := anArray indexOf: aPSVariable.
		index = 0 ifTrue: [
			anArray addLast: aPSVariable. 
			index := anArray size] ].

	aPSVariable isInheritable ifTrue: [ 
		(self inheritablePSKeys includes: index) ifFalse: [self inheritablePSKeys add: index]].

	^index
]

{ #category : 'ps - session temps' }
ProcessSpecificVariable class >> inheritablePSKeys [
	"In Pharo, this is class variable in Process"
	^SessionTemps current at: #InheritablePSKeys ifAbsentPut: [Array new]
]

{ #category : 'class initialization' }
ProcessSpecificVariable class >> initialize [
	self resetSoleInstance
]

{ #category : 'testing' }
ProcessSpecificVariable class >> isInheritable [
	^false
]

{ #category : 'instance creation' }
ProcessSpecificVariable class >> new [
	| instance |
	instance := super new.
	instance index: (self allocatePSKey: instance).
	^ instance
]

{ #category : 'ps - session temps' }
ProcessSpecificVariable class >> psKeys [
	"In Pharo, this is class variable in Process"
	^SessionTemps current at: #PSKeys ifAbsentPut: [Array new]
]

{ #category : 'ps - session temps' }
ProcessSpecificVariable class >> psKeysSemaphore [
	"In Pharo, this is class variable in Process"
	^SessionTemps current at: #PSKeysSemaphore ifAbsentPut: [Semaphore forMutualExclusion]
]

{ #category : 'class initialization' }
ProcessSpecificVariable class >> resetAllSoleInstances [
	" ProcessSpecificVariable resetAllSoleInstances "
	self resetSoleInstance.
	self resetPsKeys.
	self resetInheritablePSKeys.
	self allSubclasses do: [:ea | ea resetSoleInstance]
]

{ #category : 'ps - session temps' }
ProcessSpecificVariable class >> resetInheritablePSKeys [
	^SessionTemps current at: #InheritablePSKeys put: [Array new]
]

{ #category : 'ps - session temps' }
ProcessSpecificVariable class >> resetPsKeys [
	^SessionTemps current at: #PSKeys put: Array new
]

{ #category : 'class initialization' }
ProcessSpecificVariable class >> resetSoleInstance [
	soleInstance := nil.
]

{ #category : 'accessing' }
ProcessSpecificVariable class >> soleInstance [
	^ soleInstance ifNil: [ soleInstance := self new ]
]

{ #category : 'accessing' }
ProcessSpecificVariable class >> value [
	"Answer the current value for this variable in the current context."
	^ self soleInstance value
]

{ #category : 'process - private' }
ProcessSpecificVariable >> _growArrayInProcess: aProcess [

	| currentSize cacheSize  |
	currentSize := (self _psArrayInProcess: aProcess) size.
	cacheSize := self class psKeys size.
	currentSize < cacheSize ifTrue: [
		self 
			_psArray: ((self _psArrayInProcess: aProcess) grownBy: cacheSize - currentSize) 
			inProcess: aProcess].
]

{ #category : 'process - private' }
ProcessSpecificVariable >> _psArray: anArray inProcess: aProcess [ 

	aProcess environmentAt: #ProcessSpecificVariables put: anArray
]

{ #category : 'process - private' }
ProcessSpecificVariable >> _psArrayInProcess: aProcess [

	^aProcess 
			environmentAt: #ProcessSpecificVariables 
			ifAbsent: [aProcess environmentAt: #ProcessSpecificVariables put: (Array new: self class psKeys size)]
]

{ #category : 'process - private' }
ProcessSpecificVariable >> _psValueAt: anIndex inProcess: aProcess [

	^(aProcess environmentAt: #ProcessSpecificVariables ifAbsent: [nil]) 
		ifNotNil: [:a | a at: index ifAbsent: [] ]
]

{ #category : 'process - private' }
ProcessSpecificVariable >> _psValueAt: anIndex put: anObject inProcess: aProcess [

	self _growArrayInProcess: aProcess.
	^(self _psArrayInProcess: aProcess) at: anIndex put: anObject
]

{ #category : 'accessing' }
ProcessSpecificVariable >> default [
	"Answer the default value for the variable. The default for the default value is nil."
	^nil
]

{ #category : 'accessing' }
ProcessSpecificVariable >> index [
	^index
]

{ #category : 'accessing' }
ProcessSpecificVariable >> index: anInteger [
	index := anInteger
]

{ #category : 'inheriting' }
ProcessSpecificVariable >> isInheritable [
	^self class isInheritable
]

{ #category : 'process' }
ProcessSpecificVariable >> psValueAt: anIndex [
	"In Pharo, this logic is in Process"
	^self _psValueAt: anIndex inProcess: Processor activeProcess
]

{ #category : 'process' }
ProcessSpecificVariable >> psValueAt: anIndex put: anObject [
	"In Pharo, this logic is in Process"
	self _psValueAt: anIndex put: anObject inProcess: Processor activeProcess
]

{ #category : 'accessing' }
ProcessSpecificVariable >> value [
	"This is a hack. There may need to be something implemented to tie a value to a process in the future."

	^(self psValueAt: index) ifNil: [ self default ]
]

{ #category : 'accessing' }
ProcessSpecificVariable >> valueOrNil [
	"a faster version, which doesn't using ifAbsent: to avoid using block closure"

	^self psValueAt: index
]
