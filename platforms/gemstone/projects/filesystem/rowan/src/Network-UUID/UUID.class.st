Class {
	#name : 'UUID',
	#superclass : 'ByteArray',
	#classVars : [
		'GeneratorClass'
	],
	#category : 'Network-UUID'
}

{ #category : 'private' }
UUID class >> _generatorClass [
  ^ GeneratorClass
]

{ #category : 'instance creation' }
UUID class >> fromString: aString [
	| object |
	aString size ~= 36 ifTrue: [Error signal].
	object := self nilUUID. 
	object asUUID: aString.
	^object
]

{ #category : 'accessing' }
UUID class >> generatorClass [
  self _generatorClass ifNil: [ ^ FastUUIDGenerator ].
  ^ self _generatorClass
]

{ #category : 'accessing' }
UUID class >> generatorClass: aClass [
  GeneratorClass := aClass.
  aClass ifNotNil: [ aClass initialize ]
]

{ #category : 'instance creation' }
UUID class >> new [
  ^ (self new: 16) initialize
]

{ #category : 'instance creation' }
UUID class >> nilUUID [
	"Must call basicNew: here because I have a non-trivial initialize method."

	^self basicNew: 16
]

{ #category : 'private' }
UUID >> _primMakeUUID [
  self class generatorClass default generateBytes: self forVersion: 4
]

{ #category : 'comparing' }
UUID >> < aMagnitude [
  "Answer whether the receiver is less than the argument."

  self size = aMagnitude size
    ifFalse: [ ^ self size < aMagnitude size ].
  1 to: self size do: [ :i | 
    (self at: i) = (aMagnitude at: i)
      ifFalse: [ ^ (self at: i) < (aMagnitude at: i) ] ].
  ^ false
]

{ #category : 'comparing' }
UUID >> <= uuid [
	^ (self = uuid) or: [ self < uuid ]
]

{ #category : 'comparing' }
UUID >> > uuid [
	^ uuid < self
]

{ #category : 'comparing' }
UUID >> >= uuid [
	^ (self = uuid) or: [ uuid < self ]
]

{ #category : 'converting' }
UUID >> asInteger [
  ^ self inject: 0 into: [ :sum :each | sum * 256 + each ]
]

{ #category : 'converting' }
UUID >> asString [
	| result data |
	data := String new: 36.
	result := WriteStream on: data.
	1 to: 4 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	5 to: 6 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	7 to: 8 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	9 to: 10 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	11 to: 16 do:[:i| self printHexAt: i to: result].
	^data.
]

{ #category : 'converting' }
UUID >> asString36 [
	"Encode the UUID as a base 36 string using 0-9 and lowercase a-z.
	This is the shortest representation still being able to work as
	filenames etc since it does not depend on case nor characters
	that might cause problems, and it fits into short filenames like on
	the old MacOS HFS filesystem. The check for 36r is to make this code
	work in versions before Squeak 3.8."

	| num candidate |
	num := 0.
	1 to: self size do: [:i | num := num + ((256 raisedTo: i - 1) * (self at: i))].
	candidate := num printStringBase: 36.
	^((candidate beginsWith: '36r')
			ifTrue: [candidate copyFrom: 4 to: candidate size]
			ifFalse: [candidate]) asLowercase
]

{ #category : 'converting' }
UUID >> asUUID: aString [
	| stream token byte sz |
	stream := ReadStream on: (aString copyReplaceAll: '-' with: '') asUppercase.
	sz := stream _collection size.
	1 to: sz/2 do: [:i | 
		token := stream next: 2.
		byte := Integer fromHexString: token.
		self at: i put: byte].
	^self
]

{ #category : 'initialization' }
UUID >> initialize [
  self _primMakeUUID
]

{ #category : 'testing' }
UUID >> isNilUUID [
  1 to: self size do: [ :i | 
    (self at: i) ~= 0
      ifTrue: [ ^ false ] ].
  ^ true
]

{ #category : 'converting' }
UUID >> printHexAt: index to: aStream [
	| map v |
	map := '0123456789abcdef'.
	v := self at: index.
	aStream nextPut: (map at: (v bitShift: -4) + 1). 
	aStream nextPut: (map at: (v bitAnd: 15) + 1).
]

{ #category : 'printing' }
UUID >> printOn: aStream [
	aStream nextPutAll: 'an UUID('.
	self asString printOn: aStream.
	aStream nextPutAll: ')'
]

{ #category : 'printing' }
UUID >> printString [

	^self asString
]
