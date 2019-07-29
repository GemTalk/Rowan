"
RBPatternParser is a subclass of RBParser that allows the extended syntax that creates matching trees. These trees can be used by the ParseTreeMatcher to search and transform source code.

"
Class {
	#name : 'RBPatternParser',
	#superclass : 'RBParser',
	#category : 'AST-Core'
}

{ #category : 'private-classes' }
RBPatternParser >> messageNodeClass [
	^RBPatternMessageNode
]

{ #category : 'private-classes' }
RBPatternParser >> methodNodeClass [
	^RBPatternMethodNode
]

{ #category : 'private-parsing' }
RBPatternParser >> parseLiteralByteArrayObject [
	| node |
	(currentToken isIdentifier and: [currentToken isPatternVariable]) 
		ifTrue: 
			[node := self variableNodeClass identifierToken: currentToken.
			node isLiteralNode 
				ifTrue: 
					[self step.
					^node]].
	^super parseLiteralByteArrayObject
]

{ #category : 'private-parsing' }
RBPatternParser >> parsePatternBlock: aClass [ 
	| position node |
	position := currentToken start.
	self step.
	node := self parseBlockArgsInto: aClass new.
	node left: position.
	node body: (self parseStatements: false).
	(currentToken isSpecial and: [currentToken value = $}]) 
		ifFalse: [self parserError: '''}'' expected'].
	node right: currentToken start.
	self step.
	^node
]

{ #category : 'private-parsing' }
RBPatternParser >> parsePragmaLiteral [
  | node |
  currentToken isPatternBlock
    ifTrue: [ ^ self parsePatternBlock: RBPatternBlockNode ].
  (currentToken isIdentifier and: [ currentToken isPatternVariable ])
    ifTrue: [ 
      node := self variableNodeClass identifierToken: currentToken.
      node isLiteralNode
        ifFalse: [ self error: 'Literal pattern expected' ].
      self step.
      currentToken isPatternBlock
        ifTrue: [ 
          node := (self parsePatternBlock: RBPatternWrapperBlockNode)
            wrappedNode: node;
            yourself ].
      ^ node ].
  ^ super parsePragmaLiteral
]

{ #category : 'private-parsing' }
RBPatternParser >> parsePrimitiveLiteral [
	| node |
	(currentToken isIdentifier and: [currentToken isPatternVariable]) 
		ifTrue: 
			[node := self variableNodeClass identifierToken: currentToken.
			node isLiteralNode 
				ifTrue: 
					[self step.
					^node].
			currentToken := RBLiteralToken 
						value: currentToken value asSymbol
						start: currentToken start
						stop: currentToken stop].
	^super parsePrimitiveLiteral
]

{ #category : 'private-parsing' }
RBPatternParser >> parsePrimitiveObject [
	currentToken isPatternBlock 
		ifTrue: [^self parsePatternBlock: RBPatternBlockNode].
	^super parsePrimitiveObject
]

{ #category : 'private-parsing' }
RBPatternParser >> parseUnaryMessage [
	| node |
	node := self parsePrimitiveObject.
	self addCommentsTo: node.
	
	[currentToken isLiteralToken ifTrue: [self patchLiteralMessage].
	currentToken isPatternBlock 
		ifTrue: 
			[node := (self parsePatternBlock: RBPatternWrapperBlockNode)
						wrappedNode: node;
						yourself].
	currentToken isIdentifier] 
			whileTrue: [node := self parseUnaryMessageWith: node].
	self addCommentsTo: node.
	^node
]

{ #category : 'private' }
RBPatternParser >> patchLiteralArrayToken [
	(currentToken isIdentifier and: [currentToken isPatternVariable]) 
		ifTrue: [^self].
	super patchLiteralArrayToken
]

{ #category : 'private-classes' }
RBPatternParser >> pragmaNodeClass [
	^RBPatternPragmaNode
]

{ #category : 'accessing' }
RBPatternParser >> scannerClass [
	^RBPatternScanner
]

{ #category : 'private-classes' }
RBPatternParser >> variableNodeClass [
	^RBPatternVariableNode
]
