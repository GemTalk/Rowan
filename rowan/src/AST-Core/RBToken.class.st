"
RBToken is the abstract superclass of all of the RB tokens. These tokens (unlike the standard parser's) remember where they came from in the original source code.

Subclasses must implement the following messages:
	accessing
		length

Instance Variables:
	sourcePointer	<Integer>	The position in the original source code where this token began.

"
Class {
	#name : 'RBToken',
	#superclass : 'Object',
	#instVars : [
		'sourcePointer',
		'comments'
	],
	#category : 'AST-Core'
}

{ #category : 'instance creation' }
RBToken class >> start: anInterval [ 
	^self new start: anInterval
]

{ #category : 'accessing' }
RBToken >> comments [
	^comments
]

{ #category : 'accessing' }
RBToken >> comments: anObject [
	comments := anObject
]

{ #category : 'testing' }
RBToken >> isAssignment [
	^false
]

{ #category : 'testing' }
RBToken >> isBinary [
	^false
]

{ #category : 'testing' }
RBToken >> isIdentifier [
	^false
]

{ #category : 'testing' }
RBToken >> isKeyword [
	^false
]

{ #category : 'testing' }
RBToken >> isLiteral [
	^self isLiteralToken
]

{ #category : 'testing' }
RBToken >> isLiteralArrayToken [
	^false
]

{ #category : 'testing' }
RBToken >> isLiteralToken [
	^false
]

{ #category : 'testing' }
RBToken >> isPath [
  ^ false
]

{ #category : 'testing' }
RBToken >> isPatternBlock [
	^false
]

{ #category : 'testing' }
RBToken >> isPatternVariable [
	^false
]

{ #category : 'testing' }
RBToken >> isSpecial [
	^false
]

{ #category : 'accessing' }
RBToken >> length [
	^self subclassResponsibility
]

{ #category : 'printing' }
RBToken >> printOn: aStream [ 
	aStream
		nextPut: $ ;
		nextPutAll: self class name
]

{ #category : 'printing' }
RBToken >> rbStoreString [

"Returns a string that, when evaluated, will recreate a copy of the
 receiver.  The default is to use storeOn: to create the description."

| stream str |
str := String new.
stream := WriteStreamPortable on: str.
self rbStoreOn: stream.
^str
]

{ #category : 'accessing' }
RBToken >> removePositions [
	sourcePointer := nil
]

{ #category : 'accessing' }
RBToken >> start [
	^ sourcePointer ifNil: [ 0 ]
]

{ #category : 'initialize-release' }
RBToken >> start: anInteger [ 
	sourcePointer := anInteger
]

{ #category : 'accessing' }
RBToken >> stop [
	^ sourcePointer isNil 
		ifTrue: [ -1 ]
		ifFalse: [ self start + self length - 1 ]
]
