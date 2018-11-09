"
RBLiteralToken is the first-class representation of a literal token (entire literals, even literal arrays, are a single token in the ST80 grammar.).

Instance Variables:
	stopPosition	<Integer>	The position within the source code where the token terminates.


"
Class {
	#name : 'RBLiteralToken',
	#superclass : 'RBValueToken',
	#instVars : [
		'stopPosition'
	],
	#category : 'AST-Core-Tokens'
}

{ #category : 'instance creation' }
RBLiteralToken class >> value: anObject [ 
	| literal |
	literal := anObject class == Array 
				ifTrue: [anObject collect: [:each | self value: each]]
				ifFalse: [anObject].
	^self 
		value: literal
		start: nil
		stop: nil
]

{ #category : 'instance creation' }
RBLiteralToken class >> value: aString start: anInteger stop: stopInteger [ 
	^(self new)
		value: aString
			start: anInteger
			stop: stopInteger;
		yourself
]

{ #category : 'testing' }
RBLiteralToken >> isLiteralToken [
	^true
]

{ #category : 'testing' }
RBLiteralToken >> isMultiKeyword [
	^false
]

{ #category : 'private' }
RBLiteralToken >> length [
	^stopPosition - self start + 1
]

{ #category : 'printing' }
RBLiteralToken >> rbStoreOn: aStream [
    value isSymbol
        ifTrue: [ 
            aStream nextPut: $#.
            value asString printOn: aStream.
            ^ self ].
    value class == Character
        ifTrue: [ 
            aStream
                nextPut: $$;
                nextPut: value.
            ^ self ].
    value rbStoreOn: aStream
]

{ #category : 'accessing' }
RBLiteralToken >> realValue [
	^value
]

{ #category : 'accessing' }
RBLiteralToken >> stop: anObject [ 
	stopPosition := anObject
]

{ #category : 'initialize-release' }
RBLiteralToken >> value: aString start: anInteger stop: stopInteger [ 
	value := aString.
	sourcePointer := anInteger.
	stopPosition := stopInteger
]
