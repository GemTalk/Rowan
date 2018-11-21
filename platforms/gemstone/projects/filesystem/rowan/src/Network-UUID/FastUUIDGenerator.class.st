Class {
	#name : 'FastUUIDGenerator',
	#superclass : 'Object',
	#instVars : [
		'bits1',
		'bits2',
		'bits3',
		'bits4',
		'random',
		'semaphoreForGenerator'
	],
	#category : 'Network-UUID'
}

{ #category : 'private' }
FastUUIDGenerator class >> _defaultKey [

	^ #'UUID_DEFAULT'

]

{ #category : 'accessing' }
FastUUIDGenerator class >> default [
	^(SessionTemps current at: self _defaultKey ifAbsent: [])
		ifNil: [
			| default |
			default := self new initialize.
      SessionTemps current at: self _defaultKey put: default ]

]

{ #category : 'initialization' }
FastUUIDGenerator class >> initialize [

	SessionTemps current removeKey: self _defaultKey ifAbsent: []

]

{ #category : 'accessing' }
FastUUIDGenerator class >> next [
	"Return a UUID using the shared generator"
	
	"self next"
	
	"[ UUIDGenerator next ] bench"
	
	^ self default next
]

{ #category : 'instance creation' }
FastUUIDGenerator >> generateBytes: aPlaceHolder forVersion: aVersion [
	aVersion = 4 ifTrue: [self generateFieldsVersion4]
		ifFalse: [self error: 'Unsupported version'].
	self placeFields: aPlaceHolder.

]

{ #category : 'instance creation' }
FastUUIDGenerator >> generateFieldsVersion4 [

	semaphoreForGenerator critical: 
			[bits1 := random integer.
			bits2 := random integer.
			bits3 := random integer.
			bits4 := random integer].
	bits2 := (bits2 bitAnd: 16rFFFF0FFF) bitOr: 16r4000.
	bits3 := (bits3 bitAnd: 16r3FFFFFFF) bitOr: 16r80000000

]

{ #category : 'instance creation' }
FastUUIDGenerator >> initialize [

	random := Random new.
	semaphoreForGenerator := Semaphore forMutualExclusion

]

{ #category : 'accessing' }
FastUUIDGenerator >> next [

	"Return the next UUID"
	
	| uuid |
	uuid := UUID nilUUID.
	self generateBytes: uuid forVersion: 4.
	^ uuid
]

{ #category : 'instance creation' }
FastUUIDGenerator >> placeFields: aPlaceHolder [

	aPlaceHolder
		unsigned32At: 1 put: bits1;
		unsigned32At: 5 put: bits2;
		unsigned32At: 9 put: bits3;
		unsigned32At: 13 put: bits4

]
