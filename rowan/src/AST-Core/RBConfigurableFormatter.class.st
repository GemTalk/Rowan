"
RBConfigurableFormatter formats the Refactoring Browser's parse trees. It has many more formatting options than the default formatter used by the RB. To change the RB to use this formatter, execute ""RBProgramNode formatterClass: RBConfigurableFormatter"". For some refactorings the RB must reformat the code after the change, so it is good to have a formatter configured to your tastes.

Instance Variables:
	codeStream	<PositionableStream>	the stream we are writing our output to
	indent	<Integer>	how many times are we indenting a new line -- indents are normally tabs but could be any whitespace string
	lineStart	<Integer>	the position of the character that started the current line. This is used for calculating the line length.
	lookaheadCode	<Dictionary key: RBProgramNode value: String>	sometimes we need to lookahead while formatting, this dictionary contains the nodes that have already been formatted by lookahead
	originalSource	<String>	the original source before we started formatting. This is used to extract the comments from the original source.


"
Class {
	#name : 'RBConfigurableFormatter',
	#superclass : 'RBProgramNodeVisitor',
	#instVars : [
		'codeStream',
		'indent',
		'lookaheadCode',
		'originalSource',
		'lineStart',
		'inQueryBlock'
	],
	#classVars : [
		'CascadedMessageInsideParentheses',
		'FormatCommentWithStatements',
		'IndentsForKeywords',
		'IndentString',
		'InQueryBlock',
		'KeepBlockInMessage',
		'LineUpBlockBrackets',
		'MaxLineLength',
		'MethodSignatureOnMultipleLines',
		'MinimumNewLinesBetweenStatements',
		'MultiLineMessages',
		'NewLineAfterCascade',
		'NewLineBeforeFirstCascade',
		'NewLineBeforeFirstKeyword',
		'NewLinesAfterMethodComment',
		'NewLinesAfterMethodPattern',
		'NewLinesAfterTemporaries',
		'NumberOfArgumentsForMultiLine',
		'OneLineMessages',
		'PeriodsAtEndOfBlock',
		'PeriodsAtEndOfMethod',
		'RetainBlankLinesBetweenStatements',
		'StringFollowingReturn',
		'StringInsideBlocks',
		'StringInsideParentheses',
		'TraditionalBinaryPrecedence',
		'UseTraditionalBinaryPrecedenceForParentheses'
	],
	#category : 'AST-Core'
}

{ #category : 'accessing' }
RBConfigurableFormatter class >> cascadedMessageInsideParentheses [
	^ CascadedMessageInsideParentheses
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> cascadedMessageInsideParentheses: aBoolean [
	CascadedMessageInsideParentheses := aBoolean
]

{ #category : 'public' }
RBConfigurableFormatter class >> format: aParseTree [ 
	^self format: aParseTree withIndents: 0
]

{ #category : 'public' }
RBConfigurableFormatter class >> format: aParseTree withIndents: anInteger [ 
	^ self new
		indent: anInteger;
		format: aParseTree
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> formatCommentWithStatements [
	^ FormatCommentWithStatements
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> formatCommentWithStatements: aBoolean [
	FormatCommentWithStatements := aBoolean
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> indentsForKeywords [
	^ IndentsForKeywords
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> indentsForKeywords: anInteger [
	IndentsForKeywords := anInteger
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> indentString [
	^ IndentString
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> indentString: aString [
	IndentString := aString
]

{ #category : 'initialization' }
RBConfigurableFormatter class >> initialize [
  InQueryBlock := false.
  CascadedMessageInsideParentheses := false.
  FormatCommentWithStatements := true.
  IndentString := '	'.
  IndentsForKeywords := 1.
  KeepBlockInMessage := true.
  LineUpBlockBrackets := false.
  MaxLineLength := 80.
  MethodSignatureOnMultipleLines := false.
  MinimumNewLinesBetweenStatements := 1.
  MultiLineMessages := #(#'ifTrue:ifFalse:' #'ifFalse:ifTrue:' #'ifTrue:' #'ifFalse:' #'on:do:' #'ensure:' #'ifCurtailed:').
  NewLineAfterCascade := true.
  NewLineBeforeFirstCascade := true.
  NewLineBeforeFirstKeyword := false.
  NewLinesAfterMethodComment := 2.
  NewLinesAfterMethodPattern := 1.
  NewLinesAfterTemporaries := 1.
  NumberOfArgumentsForMultiLine := 4.
  OneLineMessages := #(#'to:' #'to:do:' #'to:by:' #'to:by:do:').
  PeriodsAtEndOfBlock := false.
  PeriodsAtEndOfMethod := false.
  RetainBlankLinesBetweenStatements := true.
  StringFollowingReturn := ' '.
  StringInsideBlocks := ' '.
  StringInsideParentheses := ''.
  TraditionalBinaryPrecedence := #(#($| $& $?) #($= $~ $< $>) #($- $+) #($* $/ $% $\) #($@)).
  UseTraditionalBinaryPrecedenceForParentheses := true
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> keepBlockInMessage [
	^ KeepBlockInMessage
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> keepBlockInMessage: aBoolean [
	KeepBlockInMessage := aBoolean
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> lineUpBlockBrackets [
	^ LineUpBlockBrackets
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> lineUpBlockBrackets: aBoolean [
	LineUpBlockBrackets := aBoolean
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> maxLineLength [
	^ MaxLineLength
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> maxLineLength: anInteger [
	MaxLineLength := anInteger
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> methodSignatureOnMultipleLines [
	^ MethodSignatureOnMultipleLines
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> methodSignatureOnMultipleLines: aBoolean [
	MethodSignatureOnMultipleLines := aBoolean
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> minimumNewLinesBetweenStatements [
	^ MinimumNewLinesBetweenStatements
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> minimumNewLinesBetweenStatements: anInteger [
	MinimumNewLinesBetweenStatements := anInteger
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> newLineAfterCascade [
	^ NewLineAfterCascade
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> newLineAfterCascade: aBoolean [
	NewLineAfterCascade := aBoolean
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> newLineBeforeFirstCascade [
	^ NewLineBeforeFirstCascade
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> newLineBeforeFirstCascade: aBoolean [
	NewLineBeforeFirstCascade := aBoolean
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> newLineBeforeFirstKeyword [
	^ NewLineBeforeFirstKeyword
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> newLineBeforeFirstKeyword: aBoolean [
	NewLineBeforeFirstKeyword := aBoolean
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> newLinesAfterMethodComment [
	^ NewLinesAfterMethodComment
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> newLinesAfterMethodComment: anInteger [
	NewLinesAfterMethodComment := anInteger
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> newLinesAfterMethodPattern [
	^ NewLinesAfterMethodPattern
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> newLinesAfterMethodPattern: anInteger [
	NewLinesAfterMethodPattern := anInteger
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> newLinesAfterTemporaries [
	^ NewLinesAfterTemporaries
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> newLinesAfterTemporaries: anInteger [
	NewLinesAfterTemporaries := anInteger
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> numberOfArgumentsForMultiLine [
	^ NumberOfArgumentsForMultiLine
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> numberOfArgumentsForMultiLine: anInteger [
	NumberOfArgumentsForMultiLine := anInteger
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> periodsAsTerminators [
	^ PeriodsAtEndOfBlock and: [ PeriodsAtEndOfMethod ]
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> periodsAsTerminators: aBoolean [
	PeriodsAtEndOfBlock := aBoolean.
	PeriodsAtEndOfMethod := aBoolean
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> periodsAtEndOfBlock [
	^ PeriodsAtEndOfBlock
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> periodsAtEndOfBlock: aBoolean [
	PeriodsAtEndOfBlock := aBoolean
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> periodsAtEndOfMethod [
	^ PeriodsAtEndOfMethod
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> periodsAtEndOfMethod: aBoolean [
	PeriodsAtEndOfMethod := aBoolean
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> retainBlankLinesBetweenStatements [
	^ RetainBlankLinesBetweenStatements
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> retainBlankLinesBetweenStatements: aBoolean [
	RetainBlankLinesBetweenStatements := aBoolean
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> stringFollowingReturn [
	^ StringFollowingReturn
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> stringFollowingReturn: aString [
	StringFollowingReturn := aString
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> stringInsideBlocks [
	^ StringInsideBlocks
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> stringInsideBlocks: aString [
	StringInsideBlocks := aString
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> stringInsideParentheses [
	^ StringInsideParentheses
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> stringInsideParentheses: aString [
	StringInsideParentheses := aString
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> useTraditionalBinaryPrecedenceForParentheses [
	^ UseTraditionalBinaryPrecedenceForParentheses
]

{ #category : 'accessing' }
RBConfigurableFormatter class >> useTraditionalBinaryPrecedenceForParentheses: aBoolean [
	UseTraditionalBinaryPrecedenceForParentheses := aBoolean
]

{ #category : 'visitor-double dispatching' }
RBConfigurableFormatter >> acceptArrayNode: anArrayNode [
	self bracketWith: '{}' around: [ self formatArray: anArrayNode ]
]

{ #category : 'visitor-double dispatching' }
RBConfigurableFormatter >> acceptAssignmentNode: anAssignmentNode [ 
	self visitNode: anAssignmentNode variable.
	codeStream space; nextPutAll: anAssignmentNode assignmentOperator; space.
	self visitNode: anAssignmentNode value
]

{ #category : 'visitor-double dispatching' }
RBConfigurableFormatter >> acceptBlockNode: aBlockNode [
	self bracketWith: '[]' around: [self formatBlock: aBlockNode]
]

{ #category : 'visitor-double dispatching' }
RBConfigurableFormatter >> acceptCascadeNode: aCascadeNode [ 
	self visitNode: aCascadeNode receiver.
	self indentAround: 
			[NewLineBeforeFirstCascade ifTrue: [self newLine] ifFalse: [self space].
			aCascadeNode messages do: 
					[:each | 
					self indentAround: 
							[self 
								formatSelectorAndArguments: each
								firstSeparator: []
								restSeparator: ((self isMultiLineMessage: each) 
										ifTrue: [[self newLine]]
										ifFalse: [[self space]])]]
				separatedBy: 
					[codeStream nextPut: $;.
					NewLineAfterCascade ifTrue: [self newLine] ifFalse: [self space]]]
]

{ #category : 'visitor-double dispatching' }
RBConfigurableFormatter >> acceptLiteralArrayNode: aRBArrayLiteralNode [ 
	| brackets |
	codeStream nextPut: $#.
	brackets := aRBArrayLiteralNode isForByteArray 
				ifTrue: ['[]']
				ifFalse: ['()'].
	self bracketWith: brackets
		around: 
			[aRBArrayLiteralNode contents do: [:each | self visitNode: each]
				separatedBy: [self space]]
]

{ #category : 'visitor-double dispatching' }
RBConfigurableFormatter >> acceptLiteralNode: aLiteralNode [ 
	self writeString: aLiteralNode token rbStoreString
]

{ #category : 'visitor-double dispatching' }
RBConfigurableFormatter >> acceptMessageNode: aMessageNode [ 
	self visitNode: aMessageNode receiver.
	self formatSelectorAndArguments: aMessageNode
]

{ #category : 'visitor-double dispatching' }
RBConfigurableFormatter >> acceptMethodNode: aMethodNode [ 
	self formatMethodPatternFor: aMethodNode.
	self formatMethodBodyFor: aMethodNode
]

{ #category : 'visitor-double dispatching' }
RBConfigurableFormatter >> acceptPatternBlockNode: aRBPatternBlockNode [ 
	codeStream nextPut: $`.
	self 
		bracketWith: '{}' 
		around: [self formatBlock: aRBPatternBlockNode]
]

{ #category : 'visitor-double dispatching' }
RBConfigurableFormatter >> acceptPatternWrapperBlockNode: aRBPatternWrapperBlockNode [
	self visitNode: aRBPatternWrapperBlockNode wrappedNode.
	codeStream nextPut: $`.
	self 
		bracketWith: '{}' 
		around: [self formatBlock: aRBPatternWrapperBlockNode]
]

{ #category : 'visitor-double dispatching' }
RBConfigurableFormatter >> acceptPragmaNode: aPragmaNode [
  codeStream nextPut: $<.
  aPragmaNode isProtected
    ifTrue: [ codeStream nextPutAll: 'protected ' ].
  self
    formatSelectorAndArguments: aPragmaNode
    firstSeparator: [ 
      aPragmaNode selector isInfix
        ifTrue: [ self space ] ]
    restSeparator: [ self space ].
  codeStream nextPut: $>
]

{ #category : 'visitor-double dispatching' }
RBConfigurableFormatter >> acceptQueryBlockNode: aBlockNode [
  self
    bracketWith: '{}'
    around: [ 
      [ 
      "query blocks are not nested"
      InQueryBlock := true.
      self formatBlock: aBlockNode ]
        ensure: [ InQueryBlock := false ] ]
]

{ #category : 'visitor-double dispatching' }
RBConfigurableFormatter >> acceptReturnNode: aReturnNode [ 
	codeStream
		nextPut: $^;
		nextPutAll: StringFollowingReturn.
	self visitNode: aReturnNode value
]

{ #category : 'visitor-double dispatching' }
RBConfigurableFormatter >> acceptSequenceNode: aSequenceNode [ 
	self formatTemporariesFor: aSequenceNode.
	self formatSequenceCommentsFor: aSequenceNode.
	self formatSequenceNodeStatementsFor: aSequenceNode
]

{ #category : 'visitor-double dispatching' }
RBConfigurableFormatter >> acceptVariableNode: aVariableNode [ 
	codeStream nextPutAll: aVariableNode name
]

{ #category : 'visitor-double dispatching' }
RBConfigurableFormatter >> acceptWorkspaceNode: aWorkspaceNode [
	self
		indentAround: [ 
			self formatMethodCommentFor: aWorkspaceNode.
			self visitNode: aWorkspaceNode body ]
]

{ #category : 'private' }
RBConfigurableFormatter >> addNewLinesBeforeStatementStartingAt: anInteger [ 
	| newLines |
	newLines := MinimumNewLinesBetweenStatements 
				max: (RetainBlankLinesBetweenStatements 
						ifTrue: [self newLinesBeforeStartingAt: anInteger]
						ifFalse: [0]).
	newLines = 0 ifTrue: [self space] ifFalse: [self newLines: newLines]
]

{ #category : 'private' }
RBConfigurableFormatter >> bracketWith: bracketString around: aBlock [ 
	bracketString isEmpty ifTrue: [^aBlock value].
	codeStream nextPut: bracketString first.
	^aBlock ensure: [codeStream nextPut: bracketString last]
]

{ #category : 'private' }
RBConfigurableFormatter >> currentLineLength [
	^codeStream position - lineStart
]

{ #category : 'public interface' }
RBConfigurableFormatter >> format: aParseTree [ 
	originalSource := aParseTree source.
	self visitNode: aParseTree.
	^codeStream contents
]

{ #category : 'private-formatting' }
RBConfigurableFormatter >> formatArray: anArrayNode [
	self formatSequenceCommentsFor: anArrayNode.
	self formatSequenceNodeStatementsFor: anArrayNode
]

{ #category : 'private-formatting' }
RBConfigurableFormatter >> formatBlock: aBlockNode [
	(LineUpBlockBrackets and: [ self willBeMultiline: aBlockNode body ])
		ifTrue: [ self newLine ]
		ifFalse: [ codeStream nextPutAll: StringInsideBlocks ].
	self formatBlockArgumentsFor: aBlockNode.
	(self willBeMultiline: aBlockNode body)
		ifTrue: [ self newLine ].
	self visitNode: aBlockNode body.
	(LineUpBlockBrackets and: [ self willBeMultiline: aBlockNode body ])
		ifTrue: [ self newLine ]
		ifFalse: [ codeStream nextPutAll: StringInsideBlocks ]
]

{ #category : 'private-formatting' }
RBConfigurableFormatter >> formatBlockArgumentsFor: aBlockNode [
	aBlockNode arguments isEmpty
		ifTrue: [ ^ self ].
	aBlockNode arguments
		do: [ :each | 
			codeStream nextPut: $:.
			self visitNode: each.
			FormatCommentWithStatements
				ifTrue: [ self formatCommentsFor: each ].
			self space ].
	codeStream nextPutAll: '| '
]

{ #category : 'private-formatting' }
RBConfigurableFormatter >> formatCommentsFor: aNode [ 
	originalSource isNil ifTrue: [^self].
	aNode comments do: 
			[:each | 
			codeStream
				space;
				nextPutAll: (originalSource copyFrom: each first to: each last)]
]

{ #category : 'private-formatting' }
RBConfigurableFormatter >> formatMethodBodyFor: aMethodNode [ 
	self
		indentAround: 
			[self newLines: NewLinesAfterMethodPattern.
			self formatMethodCommentFor: aMethodNode.
			self formatPragmasFor: aMethodNode.
			self visitNode: aMethodNode body]
]

{ #category : 'private-formatting' }
RBConfigurableFormatter >> formatMethodCommentFor: aMethodNode [
	originalSource isNil ifTrue: [^self].
	(FormatCommentWithStatements
		ifTrue: [aMethodNode methodComments]
		ifFalse: [aMethodNode comments])
		do:
			[:each | 
			codeStream
				nextPutAll:
					(originalSource
						copyFrom: each first
						to: each last).
			self newLines: NewLinesAfterMethodComment]
]

{ #category : 'private-formatting' }
RBConfigurableFormatter >> formatMethodPatternFor: aMethodNode [ 
	aMethodNode arguments isEmpty
		ifTrue: [codeStream nextPutAll: aMethodNode selector]
		ifFalse: 
			[self
				with: aMethodNode selectorParts
				and: aMethodNode arguments
				do: 
					[:key :arg | 
					codeStream nextPutAll: key value.
					self space.
					self visitNode: arg]
				separatedBy: 
					[MethodSignatureOnMultipleLines
						ifTrue: [self newLine]
						ifFalse: [self space]]]
]

{ #category : 'private-formatting' }
RBConfigurableFormatter >> formatPragmasFor: aMethodNode [
	aMethodNode pragmas do: [ :each | self visitNode: each; newLine ]
]

{ #category : 'private-formatting' }
RBConfigurableFormatter >> formatSelectorAndArguments: aMessageNode [
	| newLineBetweenArguments |
	newLineBetweenArguments := self isMultiLineMessage: aMessageNode.
	self
		indent:
			(newLineBetweenArguments
				ifTrue: [ IndentsForKeywords ]
				ifFalse: [ 0 ])
		around: [ 
			self
				formatSelectorAndArguments: aMessageNode
				firstSeparator:
					((newLineBetweenArguments or: [ NewLineBeforeFirstKeyword ])
						ifTrue: [ [ self newLine ] ]
						ifFalse: [ [ self space ] ])
				restSeparator:
					(newLineBetweenArguments
						ifTrue: [ [ self newLine ] ]
						ifFalse: [ [ self space ] ]) ]
]

{ #category : 'private-formatting' }
RBConfigurableFormatter >> formatSelectorAndArguments: aMessageNode firstSeparator: firstBlock restSeparator: restBlock [
	| separatorBlock |
	separatorBlock := firstBlock.
	aMessageNode isUnary
		ifTrue: [ 
			(self isLineTooLong: aMessageNode selector)
				ifTrue: [ self newLine ]
				ifFalse: [ separatorBlock value ].
			codeStream nextPutAll: aMessageNode selector ]
		ifFalse: [ 
			aMessageNode selectorParts
				with: aMessageNode arguments
				do: [ :selector :argument | 
					(self isLineTooLong: selector value)
						ifTrue: [ self newLine ]
						ifFalse: [ separatorBlock value ].
					separatorBlock := restBlock.
					self
						indentAround: [ 
							codeStream nextPutAll: selector value.
							(KeepBlockInMessage and: [ argument isBlock ])
								ifTrue: [ 
									self
										space;
										visitNode: argument ]
								ifFalse: [ 
									((self willBeMultiline: argument) or: [ self isLineTooLong: (self formattedSourceFor: argument) ])
										ifTrue: [ self newLine ]
										ifFalse: [ self space ].
									self visitNode: argument ] ] ] ]
]

{ #category : 'private-formatting' }
RBConfigurableFormatter >> formatSequenceCommentsFor: aSequenceNode [
	originalSource isNil ifTrue: [^self].
	aSequenceNode comments
		do:
			[:each | 
			codeStream
				nextPutAll:
					(originalSource
						copyFrom: each first
						to: each last).
			self newLine]
]

{ #category : 'private-formatting' }
RBConfigurableFormatter >> formatSequenceNodeStatementsFor: aSequenceNode [ 
	| statements |
	statements := aSequenceNode statements.
	statements isEmpty ifTrue: [^self].
	1 to: statements size
		do: 
			[:i | 
			self visitNode: (statements at: i).
			(i < statements size or: 
					[| parent |
					(parent := aSequenceNode parent) ifNil: [self class periodsAsTerminators]
						ifNotNil: 
							[ 
							parent isBlock 
								ifTrue: [self class periodsAtEndOfBlock]
								ifFalse: [self class periodsAtEndOfMethod]]]) 
				ifTrue: [codeStream nextPut: $.].
			self formatStatementCommentsFor: (statements at: i).
			i < statements size 
				ifTrue: 
					[self addNewLinesBeforeStatementStartingAt: (statements at: i + 1) start]]
]

{ #category : 'private-formatting' }
RBConfigurableFormatter >> formatStatementCommentsFor: aStatementNode [
	originalSource isNil ifTrue: [^self].
	FormatCommentWithStatements ifFalse: [^self].
	aStatementNode statementComments
		do:
			[:each | 
			codeStream
				tab;
				nextPutAll:
						(originalSource
								copyFrom: each first
								to: each last)]
]

{ #category : 'private' }
RBConfigurableFormatter >> formattedSourceFor: aNode [ 
	^lookaheadCode at: aNode
		ifAbsentPut: [self class format: aNode withIndents: indent]
]

{ #category : 'private' }
RBConfigurableFormatter >> formatTemporariesFor: aSequenceNode [
	aSequenceNode temporaries isEmpty ifTrue: [^self].
	self
		bracketWith: '|'
		around:
			[self space.
			aSequenceNode temporaries
				do:
					[:each | 
					self visitNode: each.
					FormatCommentWithStatements ifTrue: [self formatCommentsFor: each].
					self space]].
	self newLines: NewLinesAfterTemporaries
]

{ #category : 'initialize-release' }
RBConfigurableFormatter >> indent: anInteger [
	indent := anInteger
]

{ #category : 'private' }
RBConfigurableFormatter >> indent: anInteger around: aBlock [ 
	indent := indent + anInteger.
	^aBlock ensure: [indent := indent - anInteger]
]

{ #category : 'private' }
RBConfigurableFormatter >> indentAround: aBlock [ 
	self indent: 1 around: aBlock
]

{ #category : 'initialize-release' }
RBConfigurableFormatter >> initialize [
	super initialize.
	lineStart := 0.
	indent := 0.
	lookaheadCode := IdentityDictionary new.
	codeStream := WriteStreamPortable on: String new
]

{ #category : 'private' }
RBConfigurableFormatter >> isLineTooLong: aString [ 
	^ self currentLineLength + (aString indexOf: Character lf ifAbsent: [ aString size ]) >= MaxLineLength
]

{ #category : 'private-formatting' }
RBConfigurableFormatter >> isMultiLineMessage: aMessageNode [
	| messageStream |
	(MultiLineMessages includes: aMessageNode selector)
		ifTrue: [ ^ true ].
	(OneLineMessages includes: aMessageNode selector)
		ifTrue: [ ^ false ].
	(NumberOfArgumentsForMultiLine <= aMessageNode arguments size)
		ifTrue: [ ^ true ].
	(aMessageNode arguments 
		anySatisfy: [ :each | self indent: IndentsForKeywords + 1 around: [ self willBeMultiline: each ] ]) 
		ifTrue: [ ^ true ].
	aMessageNode isUnary
		ifTrue: [ ^ self isLineTooLong: aMessageNode selector ].
	messageStream := WriteStreamPortable on: String new.
	self  with: aMessageNode selectorParts and: aMessageNode arguments 
		do: [ :sel :arg | messageStream nextPutAll: sel value; space; nextPutAll: (self formattedSourceFor: arg) ]
		separatedBy: [ messageStream space ].
	^ self isLineTooLong: messageStream contents
]

{ #category : 'private' }
RBConfigurableFormatter >> needsParenthesisFor: aNode [
  | parent grandparent |
  aNode isValue
    ifFalse: [ ^ false ].
  parent := aNode parent ifNil: [ ^ false ].
  (CascadedMessageInsideParentheses
    and: [ aNode isMessage and: [ parent isMessage and: [ parent receiver == aNode ] ] ])
    ifTrue: [ 
      grandparent := parent parent.
      (grandparent notNil and: [ grandparent isCascade ])
        ifTrue: [ ^ true ] ].
  InQueryBlock
    ifTrue: [ 
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
  InQueryBlock
    ifTrue: [ ^ aNode receiver isMessage not and: [ aNode selector ~~ #'&' ] ].
  ^ UseTraditionalBinaryPrecedenceForParentheses
    and: [ self precedenceOf: parent selector greaterThan: aNode selector ]
]

{ #category : 'private' }
RBConfigurableFormatter >> newLine [
	self newLines: 1
]

{ #category : 'private' }
RBConfigurableFormatter >> newLines: anInteger [ 
	anInteger + IndentString size = 0 ifTrue: [codeStream space].
	anInteger timesRepeat: [codeStream lf].
	lineStart := codeStream position.
	indent timesRepeat: [codeStream nextPutAll: IndentString]
]

{ #category : 'private' }
RBConfigurableFormatter >> newLinesBeforeStartingAt: anIndex [ 
	| count cr lf index char |
	(anIndex isNil or: [anIndex > originalSource size]) ifTrue: [^0].
	cr := Character codePoint: 13.
	lf := Character codePoint: 10.
	count := 0.
	index := anIndex - 1.
	[index > 0 and: [(char := originalSource at: index) isSeparator]] 
		whileTrue: 
			[char == lf 
				ifTrue: 
					[count := count + 1.
					(originalSource at: (index - 1 max: 1)) == cr ifTrue: [index := index - 1]].
			char == cr ifTrue: [count := count + 1].
			index := index - 1].
	^count
]

{ #category : 'private' }
RBConfigurableFormatter >> precedenceOf: parentSelector greaterThan: childSelector [ 
	"Put parenthesis around things that are preceived to have 'lower' precedence. For example, 'a + b * c' 
	-> '(a + b) * c' but 'a * b + c' -> 'a * b + c'"

	| childIndex parentIndex |
	childIndex := 0.
	parentIndex := 0.
	1 to: TraditionalBinaryPrecedence size
		do: 
			[:i | 
			((TraditionalBinaryPrecedence at: i) includes: parentSelector first) 
				ifTrue: [parentIndex := i].
			((TraditionalBinaryPrecedence at: i) includes: childSelector first) 
				ifTrue: [childIndex := i]].
	^childIndex < parentIndex
]

{ #category : 'private' }
RBConfigurableFormatter >> space [
	codeStream space
]

{ #category : 'visiting' }
RBConfigurableFormatter >> visitNode: aNode [
	| needsParenthesis |
	(lookaheadCode includesKey: aNode)
		ifTrue: [^self writeString: (lookaheadCode at: aNode)].
	needsParenthesis := self needsParenthesisFor: aNode.
	self
		bracketWith:
			(needsParenthesis
				ifTrue: ['()']
				ifFalse: [''])
		around:
			[needsParenthesis ifTrue: [codeStream nextPutAll: StringInsideParentheses].
			super visitNode: aNode.
			(FormatCommentWithStatements or: [aNode isMethod or: [aNode isSequence]])
				ifFalse: [self formatCommentsFor: aNode].
			needsParenthesis
				ifTrue: [codeStream nextPutAll: StringInsideParentheses]]
]

{ #category : 'private' }
RBConfigurableFormatter >> willBeMultiline: aNode [ 
	^(self formattedSourceFor: aNode) includes: Character lf
]

{ #category : 'utility' }
RBConfigurableFormatter >> with: firstCollection and: secondCollection do: aBlock separatedBy: separatorBlock [ 
	firstCollection isEmpty ifTrue: [^self].
	aBlock
		value: firstCollection first
		value: secondCollection first.
	2
		to: firstCollection size
		do: 
			[:i | 
			separatorBlock value.
			aBlock
				value: (firstCollection at: i)
				value: (secondCollection at: i)]
]

{ #category : 'private' }
RBConfigurableFormatter >> writeString: aString [ 
	| index |
	index := aString lastIndexOf: Character lf startingAt: aString size ifAbsent: [0].
	codeStream nextPutAll: aString.
	index > 0 
		ifTrue: [lineStart := codeStream position - (aString size - index)]
]
