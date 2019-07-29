Class {
	#name : 'RBSmallDictionaryTest',
	#superclass : 'TestCase',
	#instVars : [
		'dict'
	],
	#category : 'AST-Tests-Core'
}

{ #category : 'running' }
RBSmallDictionaryTest >> setUp [
	super setUp.
	dict := RBSmallDictionary new
]

{ #category : 'testing' }
RBSmallDictionaryTest >> testAtError [
	dict at: #a put: 1.
	self shouldnt: [ dict at: #a ] raise: Error.
	self should: [ dict at: #b ] raise: Error
]

{ #category : 'testing' }
RBSmallDictionaryTest >> testAtIfAbsent [
	dict at: #a put: 666.
	self assert: (dict at: #a ifAbsent: [ nil ]) = 666.
	self assert: (dict at: #b ifAbsent: [ nil ]) isNil
]

{ #category : 'testing' }
RBSmallDictionaryTest >> testAtPut [
	self assert: (dict at: #a put: 3) = 3.
	self assert: (dict at: #a) = 3.
	self assert: (dict at: #a put: 4) = 4.
	self assert: (dict at: #a) = 4.
	self assert: (dict at: nil put: 5) = 5.
	self assert: (dict at: nil) = 5
]

{ #category : 'testing' }
RBSmallDictionaryTest >> testCopy [
	| copy |
	dict at: 'France' put: 'Paris'.
	dict at: 'Italie' put: 'Rome'.
	copy := dict copy.
	copy at: 'Germany' put: 'Berlin'.
	dict at: 'Switzerland' put: 'Bern'.
	self assert: copy size = 3.
	self assert: (copy includesKey: 'Germany').
	self deny: (copy includesKey: 'Switzerland').
	self assert: dict size = 3.
	self assert: (dict includesKey: 'Switzerland').
	self deny: (dict includesKey: 'Germany')
]

{ #category : 'testing' }
RBSmallDictionaryTest >> testEmpty [
	dict at: 'France' put: 'Paris'.
	dict at: 'Italie' put: 'Rome'.
	dict empty.
	self assert: dict isEmpty.
	self deny: (dict includesKey: 'France').
	self deny: (dict includesKey: 'Italie').
	self assert: dict keys isEmpty.
	self assert: dict values isEmpty
]

{ #category : 'testing' }
RBSmallDictionaryTest >> testIncludesKey [
	dict at: 'Italie' put: nil.
	dict at: 'France' put: 'Paris'.
	self assert: (dict includesKey: 'Italie').
	self assert: (dict includesKey: 'France').
	self deny: (dict includesKey: 'Switzerland')
]

{ #category : 'testing' }
RBSmallDictionaryTest >> testKeys [
	dict at: 'France' put: 'Paris'.
	dict at: 'Italie' put: 'Rome'.
	self assert: dict keys = #('France' 'Italie')
]

{ #category : 'testing' }
RBSmallDictionaryTest >> testKeysAndValuesDo [
	| keys values |
	dict at: 'France' put: 'Paris'.
	dict at: 'Italie' put: 'Rome'.
	keys := OrderedCollection new.
	values := OrderedCollection new.
	dict keysAndValuesDo: [ :key :value | 
		keys add: key.
		values add: value ].
	self assert: keys asArray = #('France' 'Italie').
	self assert: values asArray = #('Paris' 'Rome')
]

{ #category : 'testing' }
RBSmallDictionaryTest >> testKeysDo [
	| keys |
	dict at: 'France' put: 'Paris'.
	dict at: 'Italie' put: 'Rome'.
	keys := OrderedCollection new.
	dict keysDo: [ :each | keys add: each ].
	self assert: keys asArray = #('France' 'Italie')
]

{ #category : 'testing' }
RBSmallDictionaryTest >> testRemoveKey [
	dict at: #a put: 1.
	dict at: #b put: 2.
	self assert: (dict keys size) = 2.
	self assert: (dict removeKey: #a) = 1.
	self assert: (dict keys size) = 1.
	self assert: (dict at: #a ifAbsent: [ true ]).
	self assert: (dict at: #b) = 2.
	self should: [ dict removeKey: #a ] raise: Error
]

{ #category : 'testing' }
RBSmallDictionaryTest >> testRemoveKeyIfAbsent [
	dict at: #a put: 1.
	dict at: #b put: 2.
	self assert: (dict keys size) = 2.
	self assert: (dict removeKey: #a ifAbsent: [ false ]) = 1.
	self assert: (dict keys size) = 1.
	self assert: (dict at: #a ifAbsent: [ true ]).
	self assert: (dict at: #b) = 2.
	self assert: (dict removeKey: #a ifAbsent: [ true ])
]

{ #category : 'testing' }
RBSmallDictionaryTest >> testValues [
	dict at: 'France' put: 'Paris'.
	dict at: 'Italie' put: 'Rome'.
	self assert: dict values = #('Paris' 'Rome')
]

{ #category : 'testing' }
RBSmallDictionaryTest >> testValuesDo [
	| values |
	dict at: 'France' put: 'Paris'.
	dict at: 'Italie' put: 'Rome'.
	values := OrderedCollection new.
	dict valuesDo: [ :each | values add: each ].
	self assert: values asArray = #('Paris' 'Rome')
]
