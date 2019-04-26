Class {
	#name : 'RBLiteralArrayToken',
	#superclass : 'RBValueToken',
	#category : 'AST-Core-Tokens'
}

{ #category : 'testing' }
RBLiteralArrayToken >> isForByteArray [
	^value last = $[
]

{ #category : 'testing' }
RBLiteralArrayToken >> isLiteralArrayToken [
	^true
]
