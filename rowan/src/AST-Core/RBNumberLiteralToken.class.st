Class {
	#name : 'RBNumberLiteralToken',
	#superclass : 'RBLiteralToken',
	#instVars : [
		'source'
	],
	#category : 'AST-Core-Tokens'
}

{ #category : 'instance creation' }
RBNumberLiteralToken class >> value: aNumber start: anInteger stop: stopInteger source: sourceString [ 
	^(self 
		value: aNumber
		start: anInteger
		stop: stopInteger)
		source: sourceString;
		yourself
]

{ #category : 'accessing' }
RBNumberLiteralToken >> source [
	^source
]

{ #category : 'initialize-release' }
RBNumberLiteralToken >> source: aString [ 
	source := aString
]

{ #category : 'printing' }
RBNumberLiteralToken >> storeOn: aStream [ 
	aStream nextPutAll: source
]
