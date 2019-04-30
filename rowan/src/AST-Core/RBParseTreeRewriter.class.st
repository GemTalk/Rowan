"
ParseTreeRewriter walks over and transforms its RBProgramNode (tree). If the tree is modified, then answer is set to true, and the modified tree can be retrieved by the #tree method.

Instance Variables:
	tree	<RBProgramNode>	the parse tree we're transforming
"
Class {
	#name : 'RBParseTreeRewriter',
	#superclass : 'RBParseTreeSearcher',
	#instVars : [
		'tree'
	],
	#category : 'AST-Core'
}

{ #category : 'instance creation' }
RBParseTreeRewriter class >> removeTemporaryNamed: aName [ 
	| rewriteRule |
	rewriteRule := self new.
	rewriteRule replace: '| `@temps1 ' , aName , ' `@temps2 | ``@.Statements'
		with: '| `@temps1  `@temps2 | ``@.Statements'.
	^rewriteRule
]

{ #category : 'instance creation' }
RBParseTreeRewriter class >> rename: varName to: newVarName [ 
	| rewriteRule |
	rewriteRule := self new.
	rewriteRule
		replace: varName with: newVarName;
		replaceArgument: varName with: newVarName.
	^rewriteRule
]

{ #category : 'instance creation' }
RBParseTreeRewriter class >> rename: varName to: newVarName handler: aBlock [ 
	"Rename varName to newVarName, evaluating aBlock if there is a 
	temporary variable with the same name as newVarName. This 
	does not change temporary variables with varName."

	| rewriteRule |
	rewriteRule := self new.
	rewriteRule
		replace: varName with: newVarName;
		replaceArgument: newVarName
			withValueFrom: 
				[:aNode | 
				aBlock value.
				aNode].
	^rewriteRule
]

{ #category : 'accessing' }
RBParseTreeRewriter class >> replace: code with: newCode in: aParseTree [ 
	^(self 
		replace: code
		with: newCode
		method: false)
		executeTree: aParseTree;
		tree
]

{ #category : 'accessing' }
RBParseTreeRewriter class >> replace: code with: newCode in: aParseTree onInterval: anInterval [ 
	| rewriteRule |
	rewriteRule := self new.
	^rewriteRule
		replace: code
			with: newCode
			when: [:aNode | aNode intersectsInterval: anInterval];
		executeTree: aParseTree;
		tree
]

{ #category : 'instance creation' }
RBParseTreeRewriter class >> replace: code with: newCode method: aBoolean [ 
	| rewriteRule |
	rewriteRule := self new.
	aBoolean 
		ifTrue: [rewriteRule replaceMethod: code with: newCode]
		ifFalse: [rewriteRule replace: code with: newCode].
	^rewriteRule
]

{ #category : 'instance creation' }
RBParseTreeRewriter class >> replaceLiteral: literal with: newLiteral [ 
	| rewriteRule |
	rewriteRule := self new.
	rewriteRule replaceTree: (RBLiteralNode value: literal)
		withTree: (RBLiteralNode value: newLiteral).
	^rewriteRule
]

{ #category : 'accessing' }
RBParseTreeRewriter class >> replaceStatements: code with: newCode in: aParseTree onInterval: anInterval [ 
	| tree replaceStmt |
	tree := self buildTree: code method: false.
	tree isSequence 
		ifFalse: [tree := RBSequenceNode statements: (Array with: tree)].
	tree temporaries: (Array with: (RBPatternVariableNode named: '`@temps')).
	tree addNodeFirst: (RBPatternVariableNode named: '`@.S1').
	tree lastIsReturn 
		ifTrue: [replaceStmt := '| `@temps | `@.S1. ^' , newCode]
		ifFalse: 
			[tree addNode: (RBPatternVariableNode named: '`@.S2').
			replaceStmt := '| `@temps | `@.S1. ' , newCode , '. `@.S2'].
	^self 
		replace: tree formattedCode
		with: replaceStmt
		in: aParseTree
		onInterval: anInterval
]

{ #category : 'instance creation' }
RBParseTreeRewriter class >> variable: aVarName getter: getMethod setter: setMethod [ 
	^self 
		variable: aVarName
		getter: getMethod
		setter: setMethod
		receiver: 'self'
]

{ #category : 'instance creation' }
RBParseTreeRewriter class >> variable: aVarName getter: getMethod setter: setMethod receiver: aString [ 
	| rewriteRule |
	rewriteRule := self new.
	rewriteRule
		replace: aVarName , ' := ``@object'
			with: aString , ' ' , setMethod , ' ``@object';
		replace: aVarName with: aString , ' ' , getMethod.
	^rewriteRule
]

{ #category : 'visitor-double dispatching' }
RBParseTreeRewriter >> acceptArrayNode: anArrayNode [
	anArrayNode statements: (anArrayNode statements 
				collect: [:each | self visitNode: each])
]

{ #category : 'visitor-double dispatching' }
RBParseTreeRewriter >> acceptAssignmentNode: anAssignmentNode [ 
	anAssignmentNode variable: (self visitNode: anAssignmentNode variable).
	anAssignmentNode value: (self visitNode: anAssignmentNode value)
]

{ #category : 'visitor-double dispatching' }
RBParseTreeRewriter >> acceptBlockNode: aBlockNode [ 
	aBlockNode arguments: (self visitArguments: aBlockNode arguments).
	aBlockNode body: (self visitNode: aBlockNode body)
]

{ #category : 'visitor-double dispatching' }
RBParseTreeRewriter >> acceptCascadeNode: aCascadeNode [ 
	| newMessages notFound |
	newMessages := OrderedCollection new: aCascadeNode messages size.
	notFound := OrderedCollection new: aCascadeNode messages size.
	aCascadeNode messages do: 
			[:each | 
			| newNode |
			newNode := self performSearches: searches on: each.
			newNode isNil 
				ifTrue: 
					[newNode := each.
					notFound add: newNode].
			newNode isMessage 
				ifTrue: [newMessages add: newNode]
				ifFalse: 
					[newNode isCascade 
						ifTrue: [newMessages addAll: newNode messages]
						ifFalse: 
							[Transcript
								show: 'Cannot replace message node inside of cascaded node with non-message node.';
								cr.
							newMessages add: each]]].
	notFound size = aCascadeNode messages size 
		ifTrue: 
			[| receiver |
			receiver := self visitNode: aCascadeNode messages first receiver.
			newMessages do: [:each | each receiver: receiver]].
	notFound 
		do: [:each | each arguments: (each arguments collect: [:arg | self visitNode: arg])].
	aCascadeNode messages: newMessages
]

{ #category : 'visitor-double dispatching' }
RBParseTreeRewriter >> acceptLiteralArrayNode: aRBArrayLiteralNode [ 
	aRBArrayLiteralNode contents: (aRBArrayLiteralNode contents 
				collect: [:each | self visitNode: each])
]

{ #category : 'visitor-double dispatching' }
RBParseTreeRewriter >> acceptMessageNode: aMessageNode [ 
	aMessageNode receiver: (self visitNode: aMessageNode receiver).
	aMessageNode 
		arguments: (aMessageNode arguments collect: [:each | self visitNode: each])
]

{ #category : 'visitor-double dispatching' }
RBParseTreeRewriter >> acceptMethodNode: aMethodNode [ 
	aMethodNode arguments: (self visitArguments: aMethodNode arguments).
	aMethodNode pragmas: (aMethodNode pragmas 
		collect: [:each | self visitNode: each]).
	aMethodNode body: (self visitNode: aMethodNode body)
]

{ #category : 'visitor-double dispatching' }
RBParseTreeRewriter >> acceptPragmaNode: aPragmaNode [ 
	aPragmaNode arguments: (aPragmaNode arguments collect: [:each | self visitNode: each])
]

{ #category : 'visitor-double dispatching' }
RBParseTreeRewriter >> acceptReturnNode: aReturnNode [ 
	aReturnNode value: (self visitNode: aReturnNode value)
]

{ #category : 'visitor-double dispatching' }
RBParseTreeRewriter >> acceptSequenceNode: aSequenceNode [ 
	aSequenceNode 
		temporaries: (self visitArguments: aSequenceNode temporaries).
	aSequenceNode statements: (aSequenceNode statements 
				collect: [:each | self visitNode: each])
]

{ #category : 'accessing' }
RBParseTreeRewriter >> executeTree: aParseTree [ 
	| oldContext |
	oldContext := context.
	context := RBSmallDictionary new.
	answer := false.
	tree := self visitNode: aParseTree.
	context := oldContext.
	^answer
]

{ #category : 'private' }
RBParseTreeRewriter >> foundMatch [
	answer := true
]

{ #category : 'private' }
RBParseTreeRewriter >> lookForMoreMatchesInContext: oldContext [ 
	oldContext keysAndValuesDo: 
			[:key :value | 
			(key isString not and: [key recurseInto]) 
				ifTrue: 
					[oldContext at: key put: (value collect: [:each | self visitNode: each])]]
]

{ #category : 'replacing' }
RBParseTreeRewriter >> replace: searchString with: replaceString [ 
	self addRule: (RBStringReplaceRule searchFor: searchString
				replaceWith: replaceString)
]

{ #category : 'replacing' }
RBParseTreeRewriter >> replace: searchString with: replaceString when: aBlock [ 
	self addRule: (RBStringReplaceRule 
				searchFor: searchString
				replaceWith: replaceString
				when: aBlock)
]

{ #category : 'replacing' }
RBParseTreeRewriter >> replace: searchString withValueFrom: replaceBlock [ 
	self addRule: (RBBlockReplaceRule searchFor: searchString
				replaceWith: replaceBlock)
]

{ #category : 'replacing' }
RBParseTreeRewriter >> replace: searchString withValueFrom: replaceBlock when: conditionBlock [ 
	self addRule: (RBBlockReplaceRule 
				searchFor: searchString
				replaceWith: replaceBlock
				when: conditionBlock)
]

{ #category : 'replacing' }
RBParseTreeRewriter >> replaceArgument: searchString with: replaceString [ 
	self addArgumentRule: (RBStringReplaceRule searchFor: searchString
				replaceWith: replaceString)
]

{ #category : 'replacing' }
RBParseTreeRewriter >> replaceArgument: searchString with: replaceString when: aBlock [ 
	self addArgumentRule: (RBStringReplaceRule 
				searchFor: searchString
				replaceWith: replaceString
				when: aBlock)
]

{ #category : 'replacing' }
RBParseTreeRewriter >> replaceArgument: searchString withValueFrom: replaceBlock [ 
	self addArgumentRule: (RBBlockReplaceRule searchFor: searchString
				replaceWith: replaceBlock)
]

{ #category : 'replacing' }
RBParseTreeRewriter >> replaceArgument: searchString withValueFrom: replaceBlock when: conditionBlock [ 
	self addArgumentRule: (RBBlockReplaceRule 
				searchFor: searchString
				replaceWith: replaceBlock
				when: conditionBlock)
]

{ #category : 'replacing' }
RBParseTreeRewriter >> replaceMethod: searchString with: replaceString [ 
	self addRule: (RBStringReplaceRule searchForMethod: searchString
				replaceWith: replaceString)
]

{ #category : 'replacing' }
RBParseTreeRewriter >> replaceMethod: searchString with: replaceString when: aBlock [ 
	self addRule: (RBStringReplaceRule 
				searchForMethod: searchString
				replaceWith: replaceString
				when: aBlock)
]

{ #category : 'replacing' }
RBParseTreeRewriter >> replaceMethod: searchString withValueFrom: replaceBlock [ 
	self addRule: (RBBlockReplaceRule searchForMethod: searchString
				replaceWith: replaceBlock)
]

{ #category : 'replacing' }
RBParseTreeRewriter >> replaceMethod: searchString withValueFrom: replaceBlock when: conditionBlock [ 
	self addRule: (RBBlockReplaceRule 
				searchForMethod: searchString
				replaceWith: replaceBlock
				when: conditionBlock)
]

{ #category : 'replacing' }
RBParseTreeRewriter >> replaceTree: searchTree withTree: replaceTree [ 
	self addRule: (RBStringReplaceRule searchForTree: searchTree
				replaceWith: replaceTree)
]

{ #category : 'replacing' }
RBParseTreeRewriter >> replaceTree: searchTree withTree: replaceTree when: aBlock [ 
	self addRule: (RBStringReplaceRule 
				searchForTree: searchTree
				replaceWith: replaceTree
				when: aBlock)
]

{ #category : 'accessing' }
RBParseTreeRewriter >> tree [
	^tree
]

{ #category : 'visiting' }
RBParseTreeRewriter >> visitArguments: aNodeCollection [ 
	^aNodeCollection collect: [:each | self visitArgument: each]
]
