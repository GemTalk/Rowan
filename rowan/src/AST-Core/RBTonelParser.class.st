Class {
	#name : 'RBTonelParser',
	#superclass : 'RBParser',
	#category : 'AST-Core'
}

{ #category : 'accessing' }
RBTonelParser >> currentToken [

	^currentToken
]

{ #category : 'private-parsing' }
RBTonelParser >> parseTonelKeywordPattern [
	" do not process $[ token .. caller will do the right thing"

	| keywords args node continue |
	keywords := OrderedCollection new.
	args := OrderedCollection new.
	continue := true.
	[  continue and: [ currentToken isKeyword ]  ]
		whileTrue:
			[keywords add: currentToken.
			continue := (self peekTonelFor: #special value: $[) not.
			continue ifTrue: [ self step ].
			args add: self parseTonelVariableNode].
	node := self methodNodeClass
		selectorParts: keywords
		arguments: args.
	node comments: (node comments, args last comments).
	args last comments: nil.
	^node
]

{ #category : 'private-parsing' }
RBTonelParser >> parseTonelMessagePattern [

	currentToken isLiteralToken ifTrue: [self patchTonelLiteralMessage].
	^ currentToken isIdentifier 
		ifTrue: [ self parseTonelUnaryPattern]
		ifFalse: 
			[currentToken isKeyword 
				ifTrue: [self parseTonelKeywordPattern]
				ifFalse: [self parseTonelBinaryPattern] ]
]

{ #category : 'private-parsing' }
RBTonelParser >> parseTonelPragmas [
	| pragma start |
	[ currentToken isBinary and: [ currentToken value = #< ] ] whileTrue: [
		start := currentToken start.
		self step.
		pragma := self parsePragma.
		(currentToken isBinary and: [ currentToken value = #> ]) 
			ifFalse: [ self parserError: '''>'' expected' ].
		pragma left: start; right: currentToken start.
		pragmas isNil
			ifTrue: [ pragmas := OrderedCollection new ].
		pragmas addLast: pragma.
		self step ]
]

{ #category : 'private-parsing' }
RBTonelParser >> parseTonelPrimitiveIdentifier [
	| token node |
	token := currentToken.
	(self peekTonelFor: #special value: $[)
		ifFalse: [ self step ].
	node := self variableNodeClass identifierToken: token.
	self addCommentsTo: node.
	^node
]

{ #category : 'private-parsing' }
RBTonelParser >> parseTonelStatementList: pragmaBoolean into: sequenceNode [ 
	| statements return periods returnPosition node |
	return := false.
	statements := OrderedCollection new.
	periods := OrderedCollection new.
	self addCommentsTo: sequenceNode.
	pragmaBoolean ifTrue: [self parsePragmas].
	[currentToken isSpecial and: [currentToken value = $.]] whileTrue: 
		[periods add: currentToken start.
		self step].
	[self atEnd 
		or: [currentToken isSpecial and: ['])}' includes: currentToken value]]] 
			whileFalse: 
				[return ifTrue: [self parserError: 'End of statement list encounted'].
				(currentToken isSpecial and: [currentToken value = $^]) 
					ifTrue: 
						[returnPosition := currentToken start.
						self step.
						node := self returnNodeClass return: returnPosition
									value: self parseAssignment.
						statements add: node.
						return := true]
					ifFalse: 
						[node := self parseAssignment.
						statements add: node].
				(currentToken isSpecial and: [currentToken value = $.]) 
					ifTrue: 
						[periods add: currentToken start.
						self step.
						self addCommentsTo: node]
					ifFalse: [return := true].
				[currentToken isSpecial and: [currentToken value = $.]] whileTrue: 
					[periods add: currentToken start.
					self step]].
	statements notEmpty ifTrue: [self addCommentsTo: statements last].
	sequenceNode
		statements: statements;
		periods: periods.
	^sequenceNode
]

{ #category : 'private-parsing' }
RBTonelParser >> parseTonelStatements: pragmaBoolean [ 
	| args leftBar rightBar |
	args := #().
	leftBar := rightBar := nil.
	currentToken isBinary 
		ifTrue: 
			[currentToken value = #| 
				ifTrue: 
					[leftBar := currentToken start.
					self step.
					args := self parseArgs.
					(currentToken isBinary and: [currentToken value = #|]) 
						ifFalse: [self parserError: '''|'' expected'].
					rightBar := currentToken start.
					self step]
				ifFalse: 
					[currentToken value = #'||' 
						ifTrue: 
							[rightBar := (leftBar := currentToken start) + 1.
							self step]]].
	^self parseTonelStatementList: pragmaBoolean
		into: (self sequenceNodeClass 
				leftBar: leftBar
				temporaries: args
				rightBar: rightBar)
]

{ #category : 'private-parsing' }
RBTonelParser >> parseTonelUnaryPattern [
	"only used when parsing the tonel method selector line"

	| selector |
	selector := currentToken.
	^self methodNodeClass selectorParts: (Array with: selector) arguments: #()
]

{ #category : 'private-parsing' }
RBTonelParser >> parseTonelVariableNode [
	currentToken isIdentifier 
		ifFalse: [self parserError: 'Variable name expected'].
	^self parseTonelPrimitiveIdentifier
]

{ #category : 'private-parsing' }
RBTonelParser >> peekTonelFor: characterType value: characterValue [

	^ self scanner peekTonelFor: characterType value: characterValue
]

{ #category : 'accessing' }
RBTonelParser >> scanner [

	^scanner
]

{ #category : 'accessing' }
RBTonelParser >> scannerClass [
	^RBTonelScanner
]

{ #category : 'testing' }
RBTonelParser >> tonelMethodBodyTerminationToken [
	^ currentToken isSpecial and: [ currentToken value = $] ]
]

{ #category : 'private' }
RBTonelParser >> tonelStep [
	"only used when parsing the tonel method selector line"

	(currentToken notNil and: [currentToken comments notNil]) 
		ifTrue: [comments addAll: currentToken comments].
	nextToken notNil 
		ifTrue: 
			[currentToken := nextToken.
			nextToken := nil]
		ifFalse: [currentToken := scanner tonelNext]
]
