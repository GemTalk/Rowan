Class {
	#name : 'RBLiteralArrayToken',
	#superclass : 'RBValueToken',
	#category : 'AST-Core'
}

{ #category : 'testing' }
RBLiteralArrayToken >> isForByteArray [
	^value last = $[
]

{ #category : 'testing' }
RBLiteralArrayToken >> isLiteralArrayToken [
	^true
]
