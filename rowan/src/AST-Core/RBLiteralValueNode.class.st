"
RBLiteralNode is an AST node that represents literal values (e.g., #foo, true, 1, etc.), but not literal arrays.

Instance Variables:
	token	<RBLiteralToken>	the token that contains the literal value as well as its source positions
"
Class {
	#name : 'RBLiteralValueNode',
	#superclass : 'RBLiteralNode',
	#instVars : [
		'token'
	],
	#category : 'AST-Core'
}

{ #category : 'instance creation' }
RBLiteralValueNode class >> literalToken: aLiteralToken [ 
	^(self new)
		literalToken: aLiteralToken;
		yourself
]

{ #category : 'instance creation' }
RBLiteralValueNode class >> value: aValue [ 
	^self literalToken: (RBLiteralToken value: aValue)
]

{ #category : 'comparing' }
RBLiteralValueNode >> = anObject [ 
	^ super = anObject 
		and: [ self value = anObject value 
		and: [ self value species = anObject value species ] ]
]

{ #category : 'visitor' }
RBLiteralValueNode >> acceptVisitor: aProgramNodeVisitor [ 
	^aProgramNodeVisitor acceptLiteralNode: self
]

{ #category : 'matching' }
RBLiteralValueNode >> copyInContext: aDictionary [
	^ self class literalToken: self token copy removePositions
]

{ #category : 'initialize-release' }
RBLiteralValueNode >> literalToken: aLiteralToken [ 
	token := aLiteralToken
]

{ #category : 'accessing' }
RBLiteralValueNode >> startWithoutParentheses [
	^token start
]

{ #category : 'accessing' }
RBLiteralValueNode >> stopWithoutParentheses [
	^token stop
]

{ #category : 'accessing' }
RBLiteralValueNode >> token [
	^token
]

{ #category : 'accessing' }
RBLiteralValueNode >> value [
	^token realValue
]
