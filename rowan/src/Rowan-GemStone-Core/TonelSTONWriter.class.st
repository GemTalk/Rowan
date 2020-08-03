"
I'm a modified STON writer to make tonel metadata look as we want.

- it accept aliasses for classes, so I can say OrderedDictionary -> nil (then I do not have an extra information I do not want). Btw, tonel needs to use ordered dictionaries instead plain dictionaries because output needs to be deterministic, and we want to control the order of attributes we publish.
- if dictionary has just one element, it prints it in just one line, to have a more compact view.
"
Class {
	#name : 'TonelSTONWriter',
	#superclass : 'STONWriter',
	#instVars : [
		'aliases'
	],
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'accessing' }
TonelSTONWriter >> aliases [
	^ aliases

]

{ #category : 'accessing' }
TonelSTONWriter >> aliases: aDictionary [
	aliases := aDictionary

]

{ #category : 'private' }
TonelSTONWriter >> encodeKey: key value: value [

	super encodeKey: key asSymbol
		value: (value isSymbol ifTrue: [value asString] ifFalse: [value])

]

{ #category : 'writing' }
TonelSTONWriter >> encodeMap: pairs [
	| first |
	first := true.
	writeStream nextPut: ${.
	pairs isEmpty
		ifTrue: [
			self prettyPrintSpace ]
		ifFalse: [
			self indentedDo: [
				pairs size = 1 
					ifTrue: [ self prettyPrintSpace ]
					ifFalse: [ self newlineIndent ].
				pairs keysAndValuesDo: [ :key :value |
					first 
						ifTrue: [ first := false ] 
						ifFalse: [ self mapElementSeparator ].
					self encodeKey: key value: value ] ].
				pairs size = 1 
					ifTrue: [ self prettyPrintSpace ]
					ifFalse: [ self newlineIndent ] ].
	writeStream nextPut: $}

]

{ #category : 'initialization' }
TonelSTONWriter >> initialize [
	super initialize.
	self prettyPrint: true.
	aliases := { Rowan platform orderedDictionaryClass -> nil } asDictionary
]

{ #category : 'accessing' }
TonelSTONWriter >> stonNameFor: aClass [
	^ self aliases 
		at: aClass 
		ifAbsent: [ aClass stonName ]

]

{ #category : 'writing' }
TonelSTONWriter >> writeObject: anObject do: block [
	(jsonMode and: [ anObject class ~= STON listClass and: [ anObject class ~= STON mapClass ] ])
		ifTrue: [ self error: 'wrong object class for JSON mode' ].
	self with: anObject do: [
		(self stonNameFor: anObject class) ifNotNil: [ :stonName | 
			writeStream nextPutAll: stonName.
			self prettyPrintSpace ].
		block value ]

]
