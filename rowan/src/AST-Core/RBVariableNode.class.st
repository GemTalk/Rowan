"
RBVariableNode is an AST node that represent a variable (global, inst var, temp, etc.).

Instance Variables:
	token	<RBValueToken>	the token that contains our name and position


"
Class {
	#name : 'RBVariableNode',
	#superclass : 'RBValueNode',
	#instVars : [
		'token'
	],
	#category : 'AST-Core'
}

{ #category : 'instance creation' }
RBVariableNode class >> identifierToken: anIdentifierToken [ 
	^(self new)
		identifierToken: anIdentifierToken;
		yourself
]

{ #category : 'instance creation' }
RBVariableNode class >> named: aString [ 
	^self identifierToken: (RBIdentifierToken value: aString start: 0)
]

{ #category : 'comparing' }
RBVariableNode >> = anObject [ 
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	^self name = anObject name
]

{ #category : 'visitor' }
RBVariableNode >> acceptVisitor: aProgramNodeVisitor [ 
	^aProgramNodeVisitor acceptVariableNode: self
]

{ #category : 'matching' }
RBVariableNode >> copyInContext: aDictionary [ 
	^ self class identifierToken: token copy removePositions
]

{ #category : 'comparing' }
RBVariableNode >> equalTo: anObject withMapping: aDictionary [ 
	^self class = anObject class and: 
			[(aDictionary at: self name ifAbsentPut: [anObject name]) = anObject name]
]

{ #category : 'comparing' }
RBVariableNode >> hash [
	^self name hash
]

{ #category : 'initialize-release' }
RBVariableNode >> identifierToken: anIdentifierToken [ 
	token := anIdentifierToken
]

{ #category : 'testing' }
RBVariableNode >> isImmediateNode [
	^true
]

{ #category : 'testing' }
RBVariableNode >> isRead [
	^ self isWrite not and: [ self isUsed ]
]

{ #category : 'testing' }
RBVariableNode >> isVariable [
	^true
]

{ #category : 'testing' }
RBVariableNode >> isWrite [
	^ self parent notNil and: [ self parent isAssignment and: [ self parent variable == self ] ]
]

{ #category : 'accessing' }
RBVariableNode >> name [
	^token value
]

{ #category : 'testing' }
RBVariableNode >> needsParenthesis [
	^false
]

{ #category : 'accessing' }
RBVariableNode >> precedence [
	^0
]

{ #category : 'testing' }
RBVariableNode >> references: aVariableName [ 
	^self name = aVariableName
]

{ #category : 'replacing' }
RBVariableNode >> replaceSourceFrom: aNode [ 
	self addReplacement: (RBStringReplacement 
				replaceFrom: aNode start
				to: aNode stop
				with: self name)
]

{ #category : 'replacing' }
RBVariableNode >> replaceSourceWith: aNode [
	self addReplacement: (RBStringReplacement 
				replaceFrom: self start
				to: self stop
				with: aNode formattedCode)
]

{ #category : 'accessing' }
RBVariableNode >> startWithoutParentheses [
	^token start
]

{ #category : 'accessing' }
RBVariableNode >> stopWithoutParentheses [
	^token stop
]

{ #category : 'accessing' }
RBVariableNode >> token [
	^ token
]
