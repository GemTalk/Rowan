"
RBFormatter formats a parse tree. It is an example of a Visitor. This is rarely called directly. Sending 'formattedCode' to a parse tree uses this algorithm to return a pretty-printed version.

Instance Variables:
	codeStream	<PositionableStream>	The buffer where the output is accumulated.
	firstLineLength	<Integer>	The length of the first line of a message send.
	lineStart	<Integer>	The position of the current line's start.
	tabs	<Integer>	The number of tabs currently indented.


"
Class {
	#name : 'RBFormatter',
	#superclass : 'RBProgramNodeVisitor',
	#instVars : [
		'codeStream',
		'lineStart',
		'firstLineLength',
		'tabs',
		'inQueryBlock'
	],
	#category : 'AST-Core'
}

{ #category : 'visitor-double dispatching' }
RBFormatter >> acceptArrayNode: anArrayNode [
	codeStream nextPut: ${.
	anArrayNode statements isEmpty ifFalse: [
		anArrayNode statements size > 1
			ifTrue: [
				self indent: 1 while: [
					self indent.
					self formatStatementsFor: anArrayNode ].
				self indent ]
			ifFalse: [ self formatStatementsFor: anArrayNode ] ].
	codeStream nextPut: $}
]

{ #category : 'visitor-double dispatching' }
RBFormatter >> acceptAssignmentNode: anAssignmentNode [ 
	self indent: 2
		while: 
			[self visitNode: anAssignmentNode variable.
			codeStream space; nextPutAll: anAssignmentNode assignmentOperator; space.
			self visitNode: anAssignmentNode value]
]

{ #category : 'visitor-double dispatching' }
RBFormatter >> acceptBlockNode: aBlockNode [ 
	self 
		acceptBlockNode: aBlockNode
		startBlockString: '['
		endBlockString: ']'
]

{ #category : 'visitor-double dispatching' }
RBFormatter >> acceptBlockNode: aBlockNode startBlockString: startBlockString endBlockString: endBlockString [ 
	| seqNode multiline formattedBody formatter |
	seqNode := aBlockNode body.
	formatter := (self copy)
				lineStart: 0;
				yourself.
	formattedBody := formatter format: seqNode.
	multiline := self lineLength + formattedBody size > self maxLineSize 
				or: [formatter isMultiLine].
	multiline ifTrue: [self indent].
	codeStream nextPutAll: startBlockString.
	aBlockNode arguments do: 
			[:each | 
			codeStream nextPut: $:.
			self visitNode: each.
			codeStream nextPut: $ ].
	aBlockNode arguments notEmpty 
		ifTrue: 
			[codeStream nextPutAll: '| '.
			multiline ifTrue: [self indent]].
	codeStream
		nextPutAll: formattedBody;
		nextPutAll: endBlockString
]

{ #category : 'visitor-double dispatching' }
RBFormatter >> acceptCascadeNode: aCascadeNode [ 
	| messages |
	messages := aCascadeNode messages.
	self visitNode: messages first receiver.
	self indentWhile: 
			[self 
				for: messages
				do: 
					[:each | 
					self
						indent;
						indentWhile: [self formatMessage: each cascade: true]]
				separatedBy: [codeStream nextPut: $;]]
]

{ #category : 'visitor-double dispatching' }
RBFormatter >> acceptLiteralArrayNode: aRBArrayLiteralNode [ 
	codeStream nextPutAll: (aRBArrayLiteralNode isForByteArray 
				ifTrue: ['#[']
				ifFalse: ['#(']).
	self 
		for: aRBArrayLiteralNode contents
		do: [:each | self visitNode: each]
		separatedBy: [codeStream nextPut: $ ].
	codeStream 
		nextPut: (aRBArrayLiteralNode isForByteArray ifTrue: [$]] ifFalse: [$)])
]

{ #category : 'visitor-double dispatching' }
RBFormatter >> acceptLiteralNode: aLiteralNode [ 
	aLiteralNode token rbStoreOn: codeStream
]

{ #category : 'visitor-double dispatching' }
RBFormatter >> acceptMessageNode: aMessageNode [ 
	| newFormatter code |
	newFormatter := self copy.
	code := newFormatter format: aMessageNode receiver.
	codeStream nextPutAll: code.
	codeStream nextPut: $ .
	newFormatter isMultiLine
		ifTrue: [lineStart := codeStream position - newFormatter lastLineLength].
	self indent: (newFormatter isMultiLine ifTrue: [2] ifFalse: [1])
		while: [self formatMessage: aMessageNode cascade: false]
]

{ #category : 'visitor-double dispatching' }
RBFormatter >> acceptMethodNode: aMethodNode [ 
	self formatMethodPatternFor: aMethodNode.
	self indentWhile: [
		self formatMethodCommentFor: aMethodNode indentBefore: true.
		self indent.
		self formatPragmasFor: aMethodNode.
		aMethodNode body statements notEmpty 
			ifTrue: [ self visitNode: aMethodNode body ] ]
]

{ #category : 'visitor-double dispatching' }
RBFormatter >> acceptPatternBlockNode: aRBPatternBlockNode [ 
	self 
		acceptBlockNode: aRBPatternBlockNode
		startBlockString: '`{'
		endBlockString: '}'
]

{ #category : 'visitor-double dispatching' }
RBFormatter >> acceptPatternWrapperBlockNode: aRBPatternWrapperBlockNode [ 
	self visitNode: aRBPatternWrapperBlockNode wrappedNode.
	self 
		acceptBlockNode: aRBPatternWrapperBlockNode
		startBlockString: '`{'
		endBlockString: '}'
]

{ #category : 'visitor-double dispatching' }
RBFormatter >> acceptPragmaNode: aPragmaNode [
  codeStream nextPut: $<.
  aPragmaNode isProtected
    ifTrue: [ codeStream nextPutAll: 'protected ' ].
  aPragmaNode selector isInfix
    ifTrue: [ codeStream nextPut: $  ].
  self formatMessage: aPragmaNode cascade: false.
  codeStream nextPut: $>
]

{ #category : 'visitor-double dispatching' }
RBFormatter >> acceptQueryBlockNode: aBlockNode [
  [ 
  inQueryBlock := true.
  self
    acceptQueryBlockNode: aBlockNode
    startBlockString: '{'
    endBlockString: '}' ]
    ensure: [ inQueryBlock := false ]
]

{ #category : 'visitor-double dispatching' }
RBFormatter >> acceptQueryBlockNode: aBlockNode startBlockString: startBlockString endBlockString: endBlockString [
  | seqNode multiline formattedBody formatter |
  seqNode := aBlockNode body.
  formatter := self copy
    lineStart: 0;
    yourself.
  formattedBody := formatter format: seqNode.
  multiline := self lineLength + formattedBody size > self maxLineSize
    or: [ formatter isMultiLine ].
  multiline
    ifTrue: [ self indent ].
  codeStream nextPutAll: startBlockString.
  aBlockNode arguments
    do: [ :each | 
      codeStream nextPut: $:.
      self visitNode: each.
      codeStream nextPut: $  ].
  aBlockNode arguments notEmpty
    ifTrue: [ 
      codeStream nextPutAll: '| '.
      multiline
        ifTrue: [ self indent ] ].
  codeStream
    nextPutAll: formattedBody;
    nextPutAll: endBlockString
]

{ #category : 'visitor-double dispatching' }
RBFormatter >> acceptReturnNode: aReturnNode [ 
	codeStream nextPut: $^.
	self visitNode: aReturnNode value
]

{ #category : 'visitor-double dispatching' }
RBFormatter >> acceptSequenceNode: aSequenceNode [ 
	self formatMethodCommentFor: aSequenceNode indentBefore: false.
	self formatTemporariesFor: aSequenceNode.
	self formatStatementsFor: aSequenceNode
]

{ #category : 'visitor-double dispatching' }
RBFormatter >> acceptVariableNode: aVariableNode [ 
	codeStream nextPutAll: aVariableNode name
]

{ #category : 'accessing' }
RBFormatter >> firstLineLength [
	^firstLineLength isNil
		ifTrue: [codeStream position]
		ifFalse: [firstLineLength]
]

{ #category : 'private' }
RBFormatter >> for: aValue do: doBlock separatedBy: separatorBlock [ 
	"This is implemented here since IBM Smalltalk doesn't implement a do:separatedBy: method"

	aValue isEmpty ifTrue: [^self].
	1 to: aValue size - 1
		do: 
			[:i | 
			doBlock value: (aValue at: i).
			separatorBlock value].
	doBlock value: aValue last
]

{ #category : 'accessing' }
RBFormatter >> format: aNode [ 
	self visitNode: aNode.
	^codeStream contents
]

{ #category : 'private-formatting' }
RBFormatter >> formatMessage: aMessageNode cascade: cascadeBoolean [ 
	| selectorParts arguments multiLine formattedArgs indentFirst firstArgLength length |
	selectorParts := aMessageNode selectorParts.
	arguments := aMessageNode arguments.
	formattedArgs := OrderedCollection new.
	multiLine := aMessageNode selector numArgs > self maximumArgumentsPerLine.
	length := aMessageNode selector size + arguments size + 1.
	firstArgLength := 0.
	self indentWhile: 
			[1 to: arguments size
				do: 
					[:i | 
					| formatter string |
					formatter := (self copy)
								lineStart: (selectorParts at: i) length negated;
								yourself.
					string := formatter format: (arguments at: i).
					formattedArgs add: string.
					i == 1 ifTrue: [firstArgLength := formatter firstLineLength].
					length := length + string size.
					multiLine := multiLine or: [formatter isMultiLine]]].
	multiLine := multiLine or: [length + self lineLength > self maxLineSize].
	indentFirst := cascadeBoolean not and: 
					[multiLine and: 
							[(self startMessageSendOnNewLine: aMessageNode) or: 
									[self lineLength + selectorParts first length + 2 + firstArgLength 
										> self maxLineSize]]].
	indentFirst ifTrue: [self indent].
	self 
		formatMessageSelector: selectorParts
		withArguments: formattedArgs
		multiline: multiLine
]

{ #category : 'private-formatting' }
RBFormatter >> formatMessageSelector: selectorParts withArguments: formattedArgs multiline: multiLine [ 
	formattedArgs isEmpty 
		ifTrue: [codeStream nextPutAll: selectorParts first value]
		ifFalse: 
			[1 to: formattedArgs size
				do: 
					[:i | 
					(i ~= 1 and: [ multiLine not ])
						ifTrue: [codeStream nextPut: $ ].
					codeStream 
						nextPutAll: (selectorParts at: i) value;
						nextPut: $ ;
						nextPutAll: (formattedArgs at: i).
					(multiLine and: [i < formattedArgs size]) ifTrue: [self indent]]]
]

{ #category : 'private-formatting' }
RBFormatter >> formatMethodCommentFor: aNode indentBefore: aBoolean [ 
	| source |
	source := aNode source.
	source isNil ifTrue: [^self].
	aNode methodComments do: 
			[:each | 
			aBoolean ifTrue: [self indent].
			codeStream
				nextPutAll: (aNode source copyFrom: each first to: each last);
				lf.
			aBoolean ifFalse: [self indent]]
]

{ #category : 'private-formatting' }
RBFormatter >> formatMethodPatternFor: aMethodNode [ 
	| selectorParts arguments |
	selectorParts := aMethodNode selectorParts.
	arguments := aMethodNode arguments.
	arguments isEmpty 
		ifTrue: [codeStream nextPutAll: selectorParts first value]
		ifFalse: 
			[selectorParts with: arguments
				do: 
					[:selector :arg | 
					codeStream
						nextPutAll: selector value;
						nextPut: $ .
					self visitArgument: arg.
					codeStream nextPut: $ ]]
]

{ #category : 'private-formatting' }
RBFormatter >> formatPragmasFor: aNode [
	aNode pragmas do: [ :each | self visitNode: each; indent ]
]

{ #category : 'private-formatting' }
RBFormatter >> formatStatementCommentFor: aNode [ 
	| source |
	source := aNode source.
	source isNil ifTrue: [^self].
	aNode statementComments do: 
			[:each | 
			| crs |
			crs := self newLinesFor: source startingAt: each first.
			(crs - 1 max: 0) timesRepeat: [codeStream lf].
			crs == 0 ifTrue: [codeStream tab] ifFalse: [self indent].
			codeStream nextPutAll: (source copyFrom: each first to: each last)]
]

{ #category : 'private-formatting' }
RBFormatter >> formatStatementsFor: aSequenceNode [ 
	| statements |
	statements := aSequenceNode statements.
	statements isEmpty ifTrue: [^self].
	1 to: statements size - 1
		do: 
			[:i | 
			self visitNode: (statements at: i).
			codeStream nextPut: $..
			self formatStatementCommentFor: (statements at: i).
			self indent].
	self visitNode: statements last.
	self formatStatementCommentFor: statements last
]

{ #category : 'private-formatting' }
RBFormatter >> formatTemporariesFor: aSequenceNode [ 
	| temps |
	temps := aSequenceNode temporaries.
	temps isEmpty ifTrue: [^self].
	codeStream nextPutAll: '| '.
	temps do: 
			[:each | 
			self visitArgument: each.
			codeStream nextPut: $ ].
	codeStream nextPut: $|.
	self indent
]

{ #category : 'private' }
RBFormatter >> indent [
	firstLineLength isNil ifTrue: [firstLineLength := codeStream position].
	codeStream lf.
	tabs timesRepeat: [codeStream tab].
	lineStart := codeStream position
]

{ #category : 'private' }
RBFormatter >> indent: anInteger while: aBlock [ 
	tabs := tabs + anInteger.
	aBlock value.
	tabs := tabs - anInteger
]

{ #category : 'private' }
RBFormatter >> indentWhile: aBlock [ 
	self indent: 1 while: aBlock
]

{ #category : 'initialize-release' }
RBFormatter >> initialize [
  super initialize.
  codeStream := WriteStreamPortable on: String new.
  tabs := 0.
  lineStart := 0.
  inQueryBlock := false
]

{ #category : 'accessing' }
RBFormatter >> isMultiLine [
	^firstLineLength notNil
]

{ #category : 'accessing' }
RBFormatter >> lastLineLength [
	^codeStream position - (lineStart max: 0)
]

{ #category : 'private' }
RBFormatter >> lineLength [
	^codeStream position - lineStart
]

{ #category : 'private' }
RBFormatter >> lineStart: aPosition [ 
	lineStart := aPosition
]

{ #category : 'private' }
RBFormatter >> maximumArgumentsPerLine [
	^2
]

{ #category : 'private' }
RBFormatter >> maxLineSize [
	^75
]

{ #category : 'private' }
RBFormatter >> needsParenthesisFor: aNode [
  | parent grandparent |
  aNode isValue
    ifFalse: [ ^ false ].
  parent := aNode parent ifNil: [ ^ false ].
  (aNode isMessage and: [ parent isMessage and: [ parent receiver == aNode ] ])
    ifTrue: [ 
      grandparent := parent parent.
      (grandparent notNil and: [ grandparent isCascade ])
        ifTrue: [ ^ true ] ].
  inQueryBlock
    ifTrue: [ 
      aNode isLiteralNode
        ifTrue: [ 
          (parent isMessage
            and: [ parent receiver == aNode and: [ aNode parent selector == #'&' ] ])
            ifTrue: [ ^ true ] ].
      aNode isVariable
        ifTrue: [ 
          (aNode token isPath
            and: [ 
              parent isMessage
                and: [ parent receiver == aNode and: [ aNode parent selector == #'&' ] ] ])
            ifTrue: [ ^ true ] ] ].
  aNode precedence < parent precedence
    ifTrue: [ ^ false ].
  (aNode isAssignment and: [ parent isAssignment ])
    ifTrue: [ ^ false ].
  (aNode isAssignment and: [ aNode isCascade ])
    ifTrue: [ ^ true ].
  aNode precedence = 0
    ifTrue: [ ^ false ].
  aNode isMessage
    ifFalse: [ ^ true ].
  aNode precedence = parent precedence
    ifFalse: [ ^ true ].
  aNode isUnary
    ifTrue: [ ^ false ].
  aNode isKeyword
    ifTrue: [ ^ true ].
  parent receiver == aNode
    ifFalse: [ ^ true ].
  inQueryBlock
    ifTrue: [ ^ aNode receiver isMessage not and: [ aNode selector ~~ #'&' ] ].
  ^ self precedenceOf: parent selector greaterThan: aNode selector
]

{ #category : 'private-formatting' }
RBFormatter >> newLinesFor: aString startingAt: anIndex [ 
	| count cr lf index char |
	cr := Character codePoint: 13.
	lf := Character codePoint: 10.
	count := 0.
	index := anIndex - 1.
	[index > 0 and: 
			[char := aString at: index.
			char isSeparator]] 
		whileTrue: 
			[char == lf 
				ifTrue: 
					[count := count + 1.
					(aString at: (index - 1 max: 1)) == cr ifTrue: [index := index - 1]].
			char == cr ifTrue: [count := count + 1].
			index := index - 1].
	^count
]

{ #category : 'copying' }
RBFormatter >> postCopy [
	super postCopy.
	lineStart := self lineLength negated.
	codeStream := WriteStreamPortable on: String new.
	firstLineLength := nil
]

{ #category : 'private' }
RBFormatter >> precedenceOf: parentSelector greaterThan: childSelector [ 
	"Put parenthesis around things that are preceived to have 'lower' precedence. For example, 'a + b * c' 
	-> '(a + b) * c' but 'a * b + c' -> 'a * b + c'"

	| childIndex parentIndex operators |
	operators := #(#($| $& $?) #($= $~ $< $>) #($- $+) #($* $/ $% $\) #($@)).
	childIndex := 0.
	parentIndex := 0.
	1 to: operators size
		do: 
			[:i | 
			((operators at: i) includes: parentSelector first) 
				ifTrue: [parentIndex := i].
			((operators at: i) includes: childSelector first) 
				ifTrue: [childIndex := i]].
	^childIndex < parentIndex
]

{ #category : 'private' }
RBFormatter >> selectorsToLeaveOnLine [
	^#(#to:do: #to:by: #to:by:do:)
]

{ #category : 'private' }
RBFormatter >> selectorsToStartOnNewLine [
	^#(#ifTrue:ifFalse: #ifFalse:ifTrue: #ifTrue: #ifFalse:)
]

{ #category : 'testing' }
RBFormatter >> startMessageSendOnNewLine: aMessageNode [ 
	(self selectorsToStartOnNewLine includes: aMessageNode selector) 
		ifTrue: [^true].
	(self selectorsToLeaveOnLine includes: aMessageNode selector) 
		ifTrue: [^false].
	^aMessageNode selector numArgs > self maximumArgumentsPerLine
]

{ #category : 'visiting' }
RBFormatter >> visitNode: aNode [
  | parenthesis |
  parenthesis := self needsParenthesisFor: aNode.
  parenthesis
    ifTrue: [ codeStream nextPut: $( ].
  aNode acceptVisitor: self.
  parenthesis
    ifTrue: [ codeStream nextPut: $) ]
]
