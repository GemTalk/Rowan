"
RBLiteralNode is an AST node that represents literals.
"
Class {
	#name : 'RBLiteralNode',
	#superclass : 'RBValueNode',
	#category : 'AST-Core-Nodes'
}

{ #category : 'instance creation' }
RBLiteralNode class >> literalToken: aLiteralToken [ 
	^(aLiteralToken realValue class == Array 
		or: [aLiteralToken realValue class == ByteArray]) 
			ifTrue: 
				[RBLiteralArrayNode 
					startPosition: aLiteralToken start
					contents: (aLiteralToken value asArray 
							collect: [:each | RBLiteralNode literalToken: each])
					stopPosition: aLiteralToken stop
					isByteArray: aLiteralToken value class ~~ Array]
			ifFalse: [RBLiteralValueNode literalToken: aLiteralToken]
]

{ #category : 'instance creation' }
RBLiteralNode class >> value: aValue [ 
	^((aValue class == Array or: [aValue class == ByteArray]) 
		ifTrue: [RBLiteralArrayNode]
		ifFalse: [RBLiteralValueNode]) value: aValue
]

{ #category : 'comparing' }
RBLiteralNode >> = anObject [ 
	self == anObject ifTrue: [^true].
	^self class = anObject class
]

{ #category : 'comparing' }
RBLiteralNode >> hash [
	^self value hash
]

{ #category : 'testing' }
RBLiteralNode >> isImmediateNode [
	^true
]

{ #category : 'testing' }
RBLiteralNode >> isLiteralNode [
	^true
]

{ #category : 'testing' }
RBLiteralNode >> needsParenthesis [
	^false
]

{ #category : 'accessing' }
RBLiteralNode >> precedence [
	^0
]

{ #category : 'private-replacing' }
RBLiteralNode >> replaceSourceFrom: aNode [ 
	self addReplacement: (RBStringReplacement 
				replaceFrom: aNode start
				to: aNode stop
				with: self formattedCode)
]

{ #category : 'private-replacing' }
RBLiteralNode >> replaceSourceWith: aNode [ 
	self addReplacement: (RBStringReplacement 
				replaceFrom: self start
				to: self stop
				with: aNode formattedCode)
]

{ #category : 'accessing' }
RBLiteralNode >> value [
	^self subclassResponsibility
]
