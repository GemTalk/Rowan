"
RBIdentifierToken is the first class representation of an identifier token (e.g. Class)

"
Class {
	#name : 'RBIdentifierToken',
	#superclass : 'RBValueToken',
	#category : 'AST-Core-Tokens'
}

{ #category : 'testing' }
RBIdentifierToken >> isIdentifier [
	^true
]

{ #category : 'testing' }
RBIdentifierToken >> isPatternVariable [
	^value first = RBScanner patternVariableCharacter
]
