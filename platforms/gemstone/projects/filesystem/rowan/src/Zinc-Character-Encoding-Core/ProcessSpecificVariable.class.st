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

{ #category : 'class initialization' }
ProcessSpecificVariable class >> initialize [
	self resetSoleInstance.
]

{ #category : 'testing' }
ProcessSpecificVariable class >> isInheritable [
	^false
]

{ #category : 'instance creation' }
ProcessSpecificVariable class >> new [
	| instance |
	instance := super new.
	"Pharo -->  instance index: (Process allocatePSKey: instance)."
	^ instance
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

{ #category : 'accessing' }
ProcessSpecificVariable >> value [
	"This is a hack. There may need to be something implemented to tie a value to a process in the future."

	"Pharo code ^ (Processor activeProcess psValueAt: index) ifNil: [ self default ]"

	^index ifNil: [index := self default]
]

{ #category : 'accessing' }
ProcessSpecificVariable >> valueOrNil [
	"This is a hack. There may need to be something implemented to tie a value to a process in the future."

	"Phaor --> a faster version, which doesn't using ifAbsent: to avoid using block closure

	 Pharo --> ^  Processor activeProcess psValueAt: index"

	^ index
]
