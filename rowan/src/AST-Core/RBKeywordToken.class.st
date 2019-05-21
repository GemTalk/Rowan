"
RBKeywordToken is the first-class representation of a keyword token (e.g. add:)
"
Class {
	#name : 'RBKeywordToken',
	#superclass : 'RBValueToken',
	#category : 'AST-Core'
}

{ #category : 'testing' }
RBKeywordToken >> isKeyword [
	^true
]

{ #category : 'testing' }
RBKeywordToken >> isPatternVariable [
	^value first = RBScanner patternVariableCharacter
]
