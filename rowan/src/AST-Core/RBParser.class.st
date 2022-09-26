"
RBParser takes a source code string and generates an AST for it. This is a hand-written, recursive descent parser and has been optimized for speed. The simplest way to call this is either 'RBParser parseExpression: aString' if you want the AST for an expression, or 'RBParser parseMethod: aString' if you want to parse an entire method.

Instance Variables:
	currentToken	<RBToken>	The current token being processed.
	emptyStatements	<Boolean>	True if empty statements are allowed. In IBM, they are, in VW they aren't.
	errorBlock	<BlockClosure>	The block to evaluate on a syntax error.
	nextToken	<RBToken>	The next token that will be processed. This allows one-token lookahead.
	scanner	<RBScanner>	The scanner that generates a stream of tokens to parse.
	source	<String>	The source code to parse
	tags	<Collection of: Interval>	The source intervals of the tags appearing at the top of a method (e.g. Primitive calls)

Shared Variables:
	ParserType	<Symbol>	the type code we are parsing
"
Class {
	#name : 'RBParser',
	#superclass : 'Object',
	#instVars : [
		'scanner',
		'currentToken',
		'nextToken',
		'errorBlock',
		'source',
		'comments',
		'pragmas'
	],
	#category : 'AST-Core'
}

{ #category : 'instance creation' }
RBParser class >> new [

	^self basicNew initialize
]

{ #category : 'accessing' }
RBParser class >> parseExpression: aString [ 
	^self parseExpression: aString onError: nil
]

{ #category : 'accessing' }
RBParser class >> parseExpression: aString onError: aBlock [ 
	| node parser |
	parser := self new.
	parser errorBlock: aBlock.
	parser initializeParserWith: aString.
	node := parser parseExpression: aString.
	^(node statements size == 1 and: [node temporaries isEmpty]) 
		ifTrue: [node statements first]
		ifFalse: [node]
]

{ #category : 'accessing' }
RBParser class >> parseMethod: aString [ 
	^self parseMethod: aString onError: nil
]

{ #category : 'accessing' }
RBParser class >> parseMethod: aString onError: aBlock [ 
	| parser |
	parser := self new.
	parser errorBlock: aBlock.
	parser initializeParserWith: aString.
	^parser parseMethod: aString
]

{ #category : 'parsing' }
RBParser class >> parseMethodPattern: aString [ 
	| parser |
	parser := self new.
	parser errorBlock: [:error :position | ^nil].
	parser initializeParserWith: aString.
	^parser parseMessagePattern selector
]

{ #category : 'accessing' }
RBParser class >> parseRewriteExpression: aString [ 
	^self parseRewriteExpression: aString onError: nil
]

{ #category : 'accessing' }
RBParser class >> parseRewriteExpression: aString onError: aBlock [ 
	^RBPatternParser parseExpression: aString onError: aBlock
]

{ #category : 'accessing' }
RBParser class >> parseRewriteMethod: aString [ 
	^self parseRewriteMethod: aString onError: nil
]

{ #category : 'accessing' }
RBParser class >> parseRewriteMethod: aString onError: aBlock [ 
	^RBPatternParser parseMethod: aString onError: aBlock
]

{ #category : 'accessing' }
RBParser class >> parseWorkspace: aString [
	^ self parseWorkspace: aString onError: nil
]

{ #category : 'accessing' }
RBParser class >> parseWorkspace: aString onError: aBlock [
	| parser |
	parser := self new.
	parser errorBlock: aBlock.
	parser initializeParserWith: aString.
	^ parser parseWorkspace: aString
]

{ #category : 'private' }
RBParser >> addCommentsTo: aNode [
	aNode comments: aNode comments , comments.
	comments := OrderedCollection new
]

{ #category : 'private-classes' }
RBParser >> arrayNodeClass [
	^ RBArrayNode
]

{ #category : 'private-classes' }
RBParser >> assignmentNodeClass [
	^ RBAssignmentNode
]

{ #category : 'testing' }
RBParser >> atEnd [
	^currentToken class == RBToken
]

{ #category : 'private-classes' }
RBParser >> blockNodeClass [
	^ RBBlockNode
]

{ #category : 'private-classes' }
RBParser >> cascadeNodeClass [
	^ RBCascadeNode
]

{ #category : 'error handling' }
RBParser >> errorBlock [
	^errorBlock isNil ifTrue: [[:message :position | ]] ifFalse: [errorBlock]
]

{ #category : 'accessing' }
RBParser >> errorBlock: aBlock [ 
	errorBlock := aBlock.
	scanner notNil ifTrue: [scanner errorBlock: aBlock]
]

{ #category : 'error handling' }
RBParser >> errorPosition [
	^currentToken start
]

{ #category : 'initialize-release' }
RBParser >> initialize [
	comments := OrderedCollection new
]

{ #category : 'accessing' }
RBParser >> initializeParserWith: aString [ 
	source := aString.
	self scanner: (self scannerClass on: aString readStreamPortable
				errorBlock: self errorBlock)
]

{ #category : 'private-classes' }
RBParser >> literalArrayNodeClass [
	^ RBLiteralArrayNode
]

{ #category : 'private-classes' }
RBParser >> literalNodeClass [
	^ RBLiteralNode
]

{ #category : 'private-classes' }
RBParser >> messageNodeClass [
	^ RBMessageNode
]

{ #category : 'private-classes' }
RBParser >> methodNodeClass [
	^ RBMethodNode
]

{ #category : 'private' }
RBParser >> nextToken [
	^nextToken isNil ifTrue: [nextToken := scanner next] ifFalse: [nextToken]
]

{ #category : 'private-parsing' }
RBParser >> parseArgs [
	| args |
	args := OrderedCollection new.
	[currentToken isIdentifier] whileTrue: [args add: self parseVariableNode].
	^args
]

{ #category : 'private-parsing' }
RBParser >> parseArray [
  | position node |
  position := currentToken start.
  self step.
  (currentToken isSpecial and: [ currentToken value = $: ])
    ifTrue: [ ^self parseQueryBlock: position ].
  node := self arrayNodeClass new.
  node left: position.
  self parseStatementList: false into: node.
  (currentToken isSpecial and: [ currentToken value = $} ])
    ifFalse: [ self parserError: 'expected }' ].
  node right: currentToken start.
  self step.
  ^ node
]

{ #category : 'private-parsing' }
RBParser >> parseAssignment [
	"Need one token lookahead to see if we have a ':='. This method could 
	make it possible to assign the literals true, false and nil."

	| node position |
	(currentToken isIdentifier and: [self nextToken isAssignment]) 
		ifFalse: [^self parseCascadeMessage].
	node := self parseVariableNode.
	position := currentToken start.
	self step.
	^self assignmentNodeClass 
		variable: node
		value: self parseAssignment
		position: position
]

{ #category : 'private-parsing' }
RBParser >> parseBinaryMessage [
	| node |
	node := self parseUnaryMessage.
	
	[currentToken isLiteralToken ifTrue: [self patchNegativeLiteral].
	currentToken isBinary] 
			whileTrue: [node := self parseBinaryMessageWith: node].
	^node
]

{ #category : 'private-parsing' }
RBParser >> parseBinaryMessageWith: aNode [ 
	| binaryToken |
	binaryToken := currentToken.
	self step.
	^self messageNodeClass 
		receiver: aNode
		selectorParts: (Array with: binaryToken)
		arguments: (Array with: self parseUnaryMessage)
]

{ #category : 'private-parsing' }
RBParser >> parseBinaryPattern [
	| binaryToken node args |
	currentToken isBinary
		ifFalse: [self parserError: 'Message pattern expected'].
	binaryToken := currentToken.
	self step.
	args := Array with: self parseVariableNode.
	node := self methodNodeClass
		selectorParts: (Array with: binaryToken)
		arguments: args.
	node comments: node comments , args last comments.
	args last comments: nil.
	^node
]

{ #category : 'private-parsing' }
RBParser >> parseBinaryPragma [
	| binaryToken |
	currentToken isBinary 
		ifFalse: [ self parserError: 'Message pattern expected' ].
	binaryToken := currentToken.
	self step.
	^ self pragmaNodeClass
		selectorParts: (Array with: binaryToken)
		arguments: (Array with: self parsePragmaLiteral)
]

{ #category : 'private-parsing' }
RBParser >> parseBlock [
	| position node |
	position := currentToken start.
	self step.
	node := self blockNodeClass new. 
	self parseBlockArgsInto: node.
	node left: position.
	node body: (self parseStatements: false).
	(currentToken isSpecial and: [currentToken value = $]])
		ifFalse: [self parserError: ''']'' expected'].
	node right: currentToken start.
	self step.
	^node
]

{ #category : 'private-parsing' }
RBParser >> parseBlockArgsInto: node [ 
	| verticalBar args colons |
	args := OrderedCollection new: 2.
	colons := OrderedCollection new: 2.
	verticalBar := false.
	[currentToken isSpecial and: [currentToken value = $:]] whileTrue: 
			[colons add: currentToken start.
			self step.	":"
			verticalBar := true.
			args add: self parseVariableNode].
	verticalBar 
		ifTrue: 
			[currentToken isBinary 
				ifTrue: 
					[node bar: currentToken start.
					currentToken value = #| 
						ifTrue: [self step]
						ifFalse: 
							[currentToken value = #'||' 
								ifTrue: 
									["Hack the current token to be the start 
									of temps bar"

									currentToken
										value: #|;
										start: currentToken start + 1]
								ifFalse: [self parserError: '''|'' expected']]]
				ifFalse: 
					[(currentToken isSpecial and: [currentToken value = $]]) 
						ifFalse: [self parserError: '''|'' expected']]].
	node
		arguments: args;
		colons: colons.
	^node
]

{ #category : 'private-parsing' }
RBParser >> parseCascadeMessage [
	| node receiver messages semicolons |
	node := self parseKeywordMessage.
	(currentToken isSpecial 
		and: [currentToken value = $; and: [node isMessage]]) ifFalse: [^node].
	receiver := node receiver.
	messages := OrderedCollection new: 3.
	semicolons := OrderedCollection new: 3.
	messages add: node.
	[currentToken isSpecial and: [currentToken value = $;]] whileTrue: 
			[semicolons add: currentToken start.
			self step.
			messages add: (currentToken isIdentifier 
						ifTrue: [self parseUnaryMessageWith: receiver]
						ifFalse: 
							[currentToken isKeyword 
								ifTrue: [self parseKeywordMessageWith: receiver]
								ifFalse: 
									[| temp |
									currentToken isLiteralToken ifTrue: [self patchNegativeLiteral].
									currentToken isBinary ifFalse: [self parserError: 'Message expected'].
									temp := self parseBinaryMessageWith: receiver.
									temp == receiver ifTrue: [self parserError: 'Message expected'].
									temp]])].
	^self cascadeNodeClass messages: messages semicolons: semicolons
]

{ #category : 'accessing' }
RBParser >> parseExpression: aString [ 
	| node |
	node := self parseStatements: false.
	(RBMethodNode selector: #noMethod body: node) source: aString.	"Make the sequence node have a method node as its parent"
	self atEnd ifFalse: [self parserError: 'Unknown input at end'].
	^node
]

{ #category : 'private-parsing' }
RBParser >> parseKeywordMessage [
	^self parseKeywordMessageWith: self parseBinaryMessage
]

{ #category : 'private-parsing' }
RBParser >> parseKeywordMessageWith: node [ 
	| args isKeyword keywords |
	args := OrderedCollection new: 3.
	keywords := OrderedCollection new: 3.
	isKeyword := false.
	[currentToken isKeyword] whileTrue: 
			[keywords add: currentToken.
			self step.
			args add: self parseBinaryMessage.
			isKeyword := true].
	^isKeyword 
		ifTrue: 
			[self messageNodeClass 
				receiver: node
				selectorParts: keywords
				arguments: args]
		ifFalse: [node]
]

{ #category : 'private-parsing' }
RBParser >> parseKeywordPattern [
	| keywords args node |
	keywords := OrderedCollection new.
	args := OrderedCollection new.
	[currentToken isKeyword]
		whileTrue:
			[keywords add: currentToken.
			self step.
			args add: self parseVariableNode].
	node := self methodNodeClass
		selectorParts: keywords
		arguments: args.
	node comments: (node comments, args last comments).
	args last comments: nil.
	^node
]

{ #category : 'private-parsing' }
RBParser >> parseKeywordPragma [
	| keywords arguments |
	keywords := OrderedCollection new: 2.
	arguments := OrderedCollection new: 2.
	[ currentToken isKeyword ] whileTrue: [
		keywords addLast: currentToken.
		self step.
		arguments addLast: self parsePragmaLiteral ].
	^ self pragmaNodeClass selectorParts: keywords arguments: arguments
]

{ #category : 'private-parsing' }
RBParser >> parseLiteralArray [
	| stream start stop |
	start := currentToken start.
	stream := WriteStreamPortable on: Array new.
	self step.
	[self atEnd or: [currentToken isSpecial and: [currentToken value = $)]]] 
		whileFalse: [stream nextPut: self parseLiteralArrayObject].
	(currentToken isSpecial and: [currentToken value = $)]) 
		ifFalse: [self parserError: ''')'' expected'].
	stop := currentToken stop.
	self step.
	^self literalArrayNodeClass 
		startPosition: start
		contents: stream contents
		stopPosition: stop
		isByteArray: false
]

{ #category : 'private-parsing' }
RBParser >> parseLiteralArrayObject [
	currentToken isSpecial 
		ifTrue: 
			[currentToken value = $( ifTrue: [^self parseLiteralArray].
			"currentToken value == $[ ifTrue: [^self parseLiteralByteArray]"].
	currentToken isLiteralArrayToken 
		ifTrue: 
			[^currentToken isForByteArray 
				ifTrue: [self parseLiteralByteArray]
				ifFalse: [self parseLiteralArray]].
	currentToken isLiteralToken ifFalse: [self patchLiteralArrayToken].
	^self parsePrimitiveLiteral
]

{ #category : 'private-parsing' }
RBParser >> parseLiteralByteArray [
	| stream start stop |
	start := currentToken start.
	stream := WriteStreamPortable on: Array new.
	self step.
	[self atEnd or: [currentToken isSpecial and: [currentToken value = $]]]] 
		whileFalse: [stream nextPut: self parseLiteralByteArrayObject].
	(currentToken isSpecial and: [currentToken value = $]]) 
		ifFalse: [self parserError: ''']'' expected'].
	stop := currentToken stop.
	self step.
	^self literalArrayNodeClass 
		startPosition: start
		contents: stream contents
		stopPosition: stop
		isByteArray: true
]

{ #category : 'private-parsing' }
RBParser >> parseLiteralByteArrayObject [
	(currentToken isLiteralToken and: 
			[currentToken value _isInteger and: [currentToken value between: 0 and: 255]]) 
		ifFalse: [self parserError: 'Expecting 8-bit integer'].
	^self parsePrimitiveLiteral
]

{ #category : 'private-parsing' }
RBParser >> parseMessagePattern [
	currentToken isLiteralToken ifTrue: [self patchLiteralMessage].
	^currentToken isIdentifier 
		ifTrue: [self parseUnaryPattern]
		ifFalse: 
			[currentToken isKeyword 
				ifTrue: [self parseKeywordPattern]
				ifFalse: [self parseBinaryPattern]]
]

{ #category : 'private-parsing' }
RBParser >> parseMethod [
	| methodNode | 
	methodNode := self parseMessagePattern.
	self parsePragmas.
	self addCommentsTo: methodNode.
	methodNode body: (self parseStatements: true).
	pragmas isNil
		ifFalse: [ methodNode pragmas: pragmas ].
	^methodNode
]

{ #category : 'accessing' }
RBParser >> parseMethod: aString [ 
	| node |
	node := self parseMethod.
	self atEnd ifFalse: [self parserError: 'Unknown input at end'].
	node source: aString.
	^node
]

{ #category : 'private-parsing' }
RBParser >> parseNegatedNumber [
	| token |
	(self nextToken isLiteral not or: [ self nextToken realValue isNumber not ])
		ifTrue: [ self parserError: 'only numbers may be negated' ].
	token := RBLiteralToken value: self nextToken realValue negated start: currentToken start stop: nextToken stop.
	self step; step.
	^ self literalNodeClass literalToken: token
]

{ #category : 'private-parsing' }
RBParser >> parseParenthesizedExpression [
	| leftParen node |
	leftParen := currentToken start.
	self step.
	node := self parseAssignment.
	^(currentToken isSpecial and: [currentToken value = $)])
		ifTrue: 
			[node addParenthesis: (leftParen to: currentToken start).
			self step.
			node]
		ifFalse: [self parserError: ''')'' expected']
]

{ #category : 'private-parsing' }
RBParser >> parsePragma [
  ^ currentToken isIdentifier
    ifTrue: [ 
      currentToken value = 'protected'
        ifTrue: [ self parseProtectedPragma ]
        ifFalse: [ self parseUnaryPragma ] ]
    ifFalse: [ 
      currentToken isKeyword
        ifTrue: [ self parseKeywordPragma ]
        ifFalse: [ self parseBinaryPragma ] ]
]

{ #category : 'private-parsing' }
RBParser >> parsePragmaLiteral [
	^ self parseLiteralArrayObject
]

{ #category : 'private-parsing' }
RBParser >> parsePragmas [
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
RBParser >> parsePrimitiveIdentifier [
	| token node |
	token := currentToken.
	self step.
	node := self variableNodeClass identifierToken: token.
	self addCommentsTo: node.
	^node
]

{ #category : 'private-parsing' }
RBParser >> parsePrimitiveLiteral [
	| token |
	token := currentToken.
	self step.
	^self literalNodeClass literalToken: token
]

{ #category : 'private-parsing' }
RBParser >> parsePrimitiveObject [
	currentToken isIdentifier ifTrue: [^self parsePrimitiveIdentifier].
	(currentToken isLiteralToken and: [currentToken isMultiKeyword not]) 
		ifTrue: [^self parsePrimitiveLiteral].
	currentToken isLiteralArrayToken 
		ifTrue: 
			[^currentToken isForByteArray 
				ifTrue: [self parseLiteralByteArray]
				ifFalse: [self parseLiteralArray]].
	currentToken isSpecial 
		ifTrue: 
			[currentToken value = $[ ifTrue: [^self parseBlock].
			currentToken value = $( ifTrue: [^self parseParenthesizedExpression].
			currentToken value = ${ ifTrue: [^self parseArray]].
	(currentToken isBinary and: [ currentToken value = #- ]) 
		ifTrue: [ ^self parseNegatedNumber ].
	self parserError: 'Variable expected'
]

{ #category : 'private-parsing' }
RBParser >> parseProtectedPragma [
  "encoutered 'protected' as first token in pragma"

  self step.
  currentToken isKeyword
    ifFalse: [ ^ self pragmaNodeClass selectorParts: (Array with: 'protected') arguments: #() ].
  ^ self parseKeywordPragma
    isProtected: true;
    yourself
]

{ #category : 'private-parsing' }
RBParser >> parseQueryBlock: startPosition [
  | position node |
  position := startPosition.
  node := self queryBlockNodeClass new.
  self parseBlockArgsInto: node.
  node left: position.
  node body: (self parseStatements: false).
  (currentToken isSpecial and: [ currentToken value = $} ])
    ifFalse: [ self parserError: '''}'' expected' ].
  node right: currentToken start.
  self step.
  ^ node
]

{ #category : 'error handling' }
RBParser >> parserError: aString [ 
	"Evaluate the block. If it returns raise an error"

	self errorBlock value: aString value: self errorPosition.
	self error: aString
]

{ #category : 'private-parsing' }
RBParser >> parseStatementList: pragmaBoolean into: sequenceNode [ 
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
RBParser >> parseStatements: pragmaBoolean [ 
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
	^self parseStatementList: pragmaBoolean
		into: (self sequenceNodeClass 
				leftBar: leftBar
				temporaries: args
				rightBar: rightBar)
]

{ #category : 'private-parsing' }
RBParser >> parseUnaryMessage [
	| node |
	node := self parsePrimitiveObject.
	self addCommentsTo: node.
	[currentToken isLiteralToken ifTrue: [self patchLiteralMessage].
	currentToken isIdentifier] 
			whileTrue: [node := self parseUnaryMessageWith: node].
	self addCommentsTo: node.
	^node
]

{ #category : 'private-parsing' }
RBParser >> parseUnaryMessageWith: aNode [ 
	| selector |
	selector := currentToken.
	self step.
	^self messageNodeClass 
		receiver: aNode
		selectorParts: (Array with: selector)
		arguments: #()
]

{ #category : 'private-parsing' }
RBParser >> parseUnaryPattern [
	| selector |
	selector := currentToken.
	self step.
	^self methodNodeClass selectorParts: (Array with: selector) arguments: #()
]

{ #category : 'private-parsing' }
RBParser >> parseUnaryPragma [
	| selector |
	selector := currentToken.
	self step.
	^ self pragmaNodeClass selectorParts: (Array with: selector) arguments: #()
]

{ #category : 'private-parsing' }
RBParser >> parseVariableNode [
	currentToken isIdentifier 
		ifFalse: [self parserError: 'Variable name expected'].
	^self parsePrimitiveIdentifier
]

{ #category : 'accessing' }
RBParser >> parseWorkspace [
	| workspaceNode args leftBar rightBar |
	workspaceNode := self workspaceNodeClass new.
	(currentToken notNil and: [ currentToken comments notNil ])
		ifTrue: [ 
			comments addAll: currentToken comments.
			currentToken comments: nil ].
	self addCommentsTo: workspaceNode.
	args := #().
	leftBar := rightBar := nil.
	currentToken isBinary
		ifTrue: [ 
			currentToken value = #'|'
				ifTrue: [ 
					leftBar := currentToken start.
					self step.
					args := self parseArgs.
					(currentToken isBinary and: [ currentToken value = #'|' ])
						ifFalse: [ self parserError: '''|'' expected' ].
					rightBar := currentToken start.
					self step ]
				ifFalse: [ 
					currentToken value = #'||'
						ifTrue: [ 
							rightBar := (leftBar := currentToken start) + 1.
							self step ] ] ].
	workspaceNode
		body:
			(self
				parseWorkspaceStatementList: workspaceNode
				into:
					(self sequenceNodeClass leftBar: leftBar temporaries: args rightBar: rightBar)).
	^ workspaceNode
]

{ #category : 'accessing' }
RBParser >> parseWorkspace: aString [
	| node |
	node := self parseWorkspace.
	self atEnd
		ifFalse: [ self parserError: 'Unknown input at end' ].
	node source: aString.
	^ node
]

{ #category : 'accessing' }
RBParser >> parseWorkspaceStatementList: workspaceNode into: aSequenceNode [
	| statements return periods returnPosition node |
	return := false.
	statements := OrderedCollection new.
	periods := OrderedCollection new.
	self addCommentsTo: aSequenceNode.
	[ currentToken isSpecial and: [ currentToken value = $. ] ]
		whileTrue: [ 
			periods add: currentToken start.
			self step ].
	[ 
	self atEnd
		or: [ currentToken isSpecial and: [ '])}' includes: currentToken value ] ] ]
		whileFalse: [ 
			return
				ifTrue: [ self parserError: 'End of statement list encounted' ].
			(currentToken isSpecial and: [ currentToken value = $^ ])
				ifTrue: [ 
					returnPosition := currentToken start.
					self step.
					node := self returnNodeClass
						return: returnPosition
						value: self parseAssignment.
					statements add: node.
					return := true ]
				ifFalse: [ 
					node := self parseAssignment.
					statements add: node ].
			(currentToken isSpecial and: [ currentToken value = $. ])
				ifTrue: [ 
					periods add: currentToken start.
					self step.
					self addCommentsTo: node ]
				ifFalse: [ return := true ].
			[ currentToken isSpecial and: [ currentToken value = $. ] ]
				whileTrue: [ 
					periods add: currentToken start.
					self step ] ].
	self addCommentsTo: workspaceNode.
	aSequenceNode
		statements: statements;
		periods: periods.
	^ aSequenceNode
]

{ #category : 'private' }
RBParser >> patchLiteralArrayToken [
	(currentToken isIdentifier and: 
			[self nextToken isAssignment 
				and: [currentToken stop + 1 = self nextToken start]]) 
		ifTrue: 
			[currentToken := RBLiteralToken 
						value: (currentToken value , ':') asSymbol
						start: currentToken start
						stop: self nextToken start.
			nextToken := RBLiteralToken 
						value: #=
						start: nextToken stop
						stop: nextToken stop.
			^self].
	currentToken isAssignment 
		ifTrue: 
			[currentToken := RBLiteralToken 
						value: #':'
						start: currentToken start
						stop: currentToken start.
			nextToken := RBLiteralToken 
						value: #=
						start: currentToken stop
						stop: currentToken stop.
			^self].
	currentToken isSpecial 
		ifTrue: 
			[currentToken := RBLiteralToken 
						value: (String with: currentToken value) asSymbol
						start: currentToken start
						stop: currentToken stop.
			^self].
	(currentToken isIdentifier and: [currentToken value includes: $.]) 
		ifTrue: 
			[currentToken := RBLiteralToken 
						value: currentToken value
						start: currentToken start
						stop: currentToken stop.
			^self].
	(currentToken isIdentifier 
		or: [currentToken isBinary or: [currentToken isKeyword]]) 
			ifFalse: [^self parserError: 'Invalid token'].
	currentToken := RBLiteralToken 
				value: currentToken value asSymbol
				start: currentToken start
				stop: currentToken stop
]

{ #category : 'private' }
RBParser >> patchLiteralMessage [
	currentToken value == true 
		ifTrue: 
			[^currentToken := RBIdentifierToken value: 'true' start: currentToken start].
	currentToken value == false 
		ifTrue: 
			[^currentToken := RBIdentifierToken value: 'false' start: currentToken start].
	currentToken value == nil 
		ifTrue: 
			[^currentToken := RBIdentifierToken value: 'nil' start: currentToken start]
]

{ #category : 'private' }
RBParser >> patchNegativeLiteral [
	"Handle the special negative number case for binary message sends."

	currentToken value isNumber 
		ifFalse: [^self].
	currentToken value <= 0 ifFalse: [^self].
	currentToken value = 0 
		ifTrue: 
			[(source notNil and: 
					[source notEmpty 
						and: [(source at: (currentToken start min: source size)) = $-]]) 
				ifFalse: [^self]].
	nextToken := currentToken.
	currentToken := RBBinarySelectorToken value: #- start: nextToken start.
	nextToken value: nextToken value negated.
	(nextToken isKindOf: RBNumberLiteralToken) 
		ifTrue: 
			[nextToken source: (nextToken source copyFrom: 2 to: nextToken source size)].
	nextToken start: nextToken start + 1
]

{ #category : 'private-classes' }
RBParser >> pragmaNodeClass [
	^ RBPragmaNode
]

{ #category : 'private-classes' }
RBParser >> queryBlockNodeClass [
  ^ RBQueryBlockNode
]

{ #category : 'private-classes' }
RBParser >> returnNodeClass [
	^ RBReturnNode
]

{ #category : 'initialize-release' }
RBParser >> scanner: aScanner [ 
	scanner := aScanner.
	pragmas := nil.
	self initialize.
	self step
]

{ #category : 'accessing' }
RBParser >> scannerClass [
	^RBScanner
]

{ #category : 'private-classes' }
RBParser >> sequenceNodeClass [
	^ RBSequenceNode
]

{ #category : 'private' }
RBParser >> step [
	(currentToken notNil and: [currentToken comments notNil]) 
		ifTrue: [comments addAll: currentToken comments].
	nextToken notNil 
		ifTrue: 
			[currentToken := nextToken.
			nextToken := nil]
		ifFalse: [currentToken := scanner next]
]

{ #category : 'private-classes' }
RBParser >> variableNodeClass [
	^ RBVariableNode
]

{ #category : 'private-classes' }
RBParser >> workspaceNodeClass [
	^ RBWorkspaceNode
]
