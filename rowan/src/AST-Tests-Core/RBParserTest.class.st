Class {
	#name : 'RBParserTest',
	#superclass : 'TestCase',
	#category : 'AST-Tests-Core'
}

{ #category : 'accessing' }
RBParserTest >> compare: anObject to: anotherObject [ 
	self assert: anObject hash = anotherObject hash.
	self assert: anObject = anotherObject
]

{ #category : 'accessing' }
RBParserTest >> exampleClasses [
	^ Array with: RBParser with: RBScanner with: RBProgramNode with: RBConfigurableFormatter
]

{ #category : 'private' }
RBParserTest >> parseError: each [
	RBParser 
		parseExpression: each first 
		onError: [ :string :pos | ^ self assert: pos = each last ].
	self assert: false description: 'Parser didn''t fail'
]

{ #category : 'tests' }
RBParserTest >> testAmbiguity [
	"this should be two statements ..."

	| tree |
	tree := RBParser parseExpression: '1. 1'.
	self assert: tree statements size = 2.
	self assert: tree periods size = 1.
	tree statements do: [ :each | each value == 1 ]
]

{ #category : 'tests' }
RBParserTest >> testArray [
	| tree |
	#(('{}' 0 0) ('{.}' 0 1) ('{..}' 0 2) ('{foo. bar}' 2 1) ('{foo. bar.}' 2 2) ('{foo. bar. .}' 2 3) ('{. foo. bar}' 2 2) ('{foo.. bar}' 2 2)) do: [ :each |
		tree := RBParser parseExpression: each first.
		self assert: tree statements size = each second.
		self assert: tree periods size = each last.
		self assert: tree left = 1.
		self assert: tree right = each first size ]
]

{ #category : 'tests' }
RBParserTest >> testBestNodeFor [
	| tree |
	tree := self treeWithReallyEverything.
	tree nodesDo: 
			[:each | 
			each sourceInterval isEmpty 
				ifFalse: 
					[self assert: ((tree bestNodeFor: each sourceInterval) = each 
								or: [each parent isCascade and: [each parent messages last = each]])]]
]

{ #category : 'tests' }
RBParserTest >> testBlockRewrites [
	| rewriter tree |
	tree := RBParser 
				parseMethod: 'method: asdf
	<primitive: 1>
	<primitive: 2>
	^asdf + self foo + asdf'.
	rewriter := RBParseTreeRewriter new.
	rewriter 
		replace: 'asdf'
		with: 'fdsa'
		when: [:aNode | aNode parent parent isReturn].
	rewriter replace: 'self foo'
		withValueFrom: [:aNode | RBVariableNode named: aNode selector asString].
	rewriter 
		replaceArgument: 'asdf'
		withValueFrom: [:aNode | RBVariableNode named: 'xxx']
		when: [:aNode | false].
	rewriter executeTree: tree.
	self compare: tree
		to: (RBParser 
				parseMethod: 'method: asdf
	<primitive: 1>
	<primitive: 2>
	^asdf +  foo + fdsa')
]

{ #category : 'tests' }
RBParserTest >> testCascadeReplacement [
	| cascade |
	cascade := RBParser parseExpression: 'self foo; bar; baz'.
	(cascade messages at: 2) 
		replaceWith: (RBParser parseExpression: 'self bar: 2').
	self compare: cascade
		to: (RBParser parseExpression: 'self foo; bar: 2; baz')
]

{ #category : 'tests' }
RBParserTest >> testComparingTrees [
	self compare: self treeWithEverything to: self treeWithEverything.
	self compare: self treeWithReallyEverything to: self treeWithReallyEverything.
	
	self exampleClasses do: [ :class |
		class selectors do: [ :selector | 
			self
				compare: (class parseTreeFor: selector) 
				to: (class parseTreeFor: selector) ] ]
]

{ #category : 'tests' }
RBParserTest >> testCopy [
	| tree |
	tree := self treeWithEverything.
	self compare: tree to: tree copy.
	tree := self treeWithReallyEverything.
	self compare: tree to: tree copy.
	
	self exampleClasses do: [ :class |
		class selectors do: [ :each |
			tree := class parseTreeFor: each.
			self compare: tree to: tree copy ] ]
]

{ #category : 'tests' }
RBParserTest >> testCopyInContext [
	| tree |
	tree := self treeWithEverything.
	self compare: tree to: (tree copyInContext: Dictionary new).
	tree := self treeWithReallyEverything.
	self compare: tree to: (tree copyInContext: Dictionary new).
	
	self exampleClasses do: [ :class |
		class selectors do: [ :each |
			tree := class parseTreeFor: each.
			self compare: tree to: (tree copyInContext: Dictionary new) ] ]
]

{ #category : 'tests' }
RBParserTest >> testCreationProtocol [
	| messageNode |
	self compare: (RBMessageNode 
				receiver: (RBVariableNode named: 'self')
				selector: #+
				arguments: (Array with: (RBLiteralNode value: 0)))
		to: (RBParser parseExpression: 'self + 0').
	messageNode := RBMessageNode receiver: (RBVariableNode named: 'self')
				selector: #foo.
	self compare: (RBMethodNode selector: #bar
				body: (RBSequenceNode statements: (OrderedCollection 
								with: (RBCascadeNode messages: (OrderedCollection with: messageNode with: messageNode)))))
		to: (RBParser parseMethod: 'bar self foo; foo')
]

{ #category : 'tests' }
RBParserTest >> testEqualToWithMapping [
	| tree |
	tree := self treeWithEverything.
	self assert: (tree equalTo: tree withMapping: Dictionary new).
	tree := self treeWithReallyEverything.
	self assert: (tree equalTo: tree withMapping: Dictionary new).
	
	self exampleClasses do: [ :class |
		class selectors do: [ :each |
			tree := class parseTreeFor: each.
			self assert: (tree equalTo: tree withMapping: Dictionary new) ] ]
]

{ #category : 'tests' }
RBParserTest >> testEquivalentExceptRenaming [
	#(('a 3-4' 'a 4-3' false ) ('a #[3 4]' 'a #(3 4)' false ) ('a variable1 ~~ "comment" variable2' 'a variable1 ~~ variable2' true ) ('a variable1' 'a variable2' false ) ('a [:a :b | a + b]' 'a [:b :a | a + b]' false ) ('a | a b | a + b' 'a | b a | a + b' true ) ('a | a | a msg1; msg2' 'a | b | b msg2; msg2' false ) ('a c' 'a d' true ) ('a | a b | a := b. ^b msg1' 'a | a b | b := a. ^a msg1' true ) ('a | a b | a := b. ^b msg1: a' 'a | a b | b := a. ^b msg1: a' false ) ('a: b b + 4' 'a: e e + 4' true ) ('a: b b + 4' 'b: b b + 4' false ) ('a: b b: c b + c' 'a: c b: b c + b' true ) ('a: a b: b a + b' 'a: b b: a a + b' false ) ) do: [:each | self assert: ((RBParser parseMethod: each first)
				equalTo: (RBParser parseMethod: (each at: 2))
				exceptForVariables: #('c' ))
				== each last ]
]

{ #category : 'tests' }
RBParserTest >> testFormatter [
	self exampleClasses do: [ :class |
		class selectors do: [ :selector | 
		self 
			compare: (class parseTreeFor: selector)
			to: (RBParser parseMethod: (class parseTreeFor: selector) printString) ] ]
]

{ #category : 'tests' }
RBParserTest >> testGsNumberParsing [
  | numbers node |
  numbers := #(#('16#11' 16#11)).
  numbers
    do: [ :spec | 
      node := RBParser parseExpression: spec first.
      self assert: node token source = spec first.
      self assert: node value = spec second ]
]

{ #category : 'tests' }
RBParserTest >> testIntervals [
	| tree |
	tree := self treeWithReallyEverything.
	tree nodesDo: 
			[:each | 
			(each parent isNil or: [each parent isCascade not and: [ each parent isLiteral not]]) 
				ifTrue: 
					[| newNode source |
					source := tree source copyFrom: each start to: each stop.
					each isPragma ifFalse: [
						newNode := each isMethod 
								ifTrue: [RBParser parseMethod: source]
								ifFalse: [RBParser parseExpression: source].
						self compare: each to: newNode]]]
]

{ #category : 'tests' }
RBParserTest >> testIsA [
	| nodes types |
	nodes := Bag new.
	types := Set new.
	#(#(#isAssignment 1) #(#isBlock 1) #(#isCascade 1) #(#isLiteral 2) #(#isMessage 3) #(#isMethod 1) #(#isReturn 1) #(#isSequence 2) #(#isValue 15) #(#isVariable 7) #(#isUsed 10) #(#isDirectlyUsed 9) #(#hasParentheses 1) #(#isBinary 0) #(#isPrimitive 0) #(#isImmediate 10) #(#isWrite 1) #(#isRead 3)) 
		do: 
			[:each | 
			each last timesRepeat: [nodes add: each first].
			types add: each first].
	self treeWithEverything nodesDo: 
			[:each | 
			types do: 
					[:sel | 
					((each respondsTo: sel) and: [each perform: sel]) 
						ifTrue: [nodes remove: sel]]].
	self assert: nodes isEmpty
]

{ #category : 'tests' }
RBParserTest >> testLiteralArray [
	| tree collection |
	tree := RBParser parseExpression: '#( a #b #''c'' . + - 1 -2 3.4 #true true #false false #nil nil "comment" ''string'' #[ 1 2 3 ] #(1 2 3))'.
	collection := OrderedCollection new.
	collection add: #a; add: #b; add: #c; add: #'.'; add: #+; add: #-; add: 1; add: -2; add: 3.4; add: #true; add: true; add: #false; add: false; add: #nil; add: nil; add: 'string'; add: #[1 2 3]; add: #(1 2 3).	
	tree value with: collection do: [ :token :value | 
		self assert: token value = value ]
]

{ #category : 'tests' }
RBParserTest >> testLiteralIntevals [
	| tree |
	tree := RBParser parseExpression: '#(#a b #( c ))'.
	self assert: tree contents first start = 3.
	self assert: tree contents first stop = 4.
	self assert: tree contents last contents first start = 11
]

{ #category : 'tests' }
RBParserTest >> testMatchInContext [
	| tree |
	tree := self treeWithEverything.
	self assert: (tree match: tree inContext: Dictionary new).
	tree := self treeWithReallyEverything.
	self assert: (tree match: tree inContext: Dictionary new).
	
	self exampleClasses do: [ :class |
		class selectors do: [ :each |
			tree := class parseTreeFor: each.
			self assert: (tree match: tree inContext: Dictionary new) ] ]
]

{ #category : 'tests' }
RBParserTest >> testMethodPatterns [
	#(#('+ a ^self + a' #+) #('foo ^self foo' #foo) #('foo: a bar: b ^a + b' #foo:bar:)) 
		do: [:each | self assert: (RBParser parseMethodPattern: each first) == each last]
]

{ #category : 'tests' }
RBParserTest >> testModifying [
	| tree |
	tree := RBParser 
				parseMethod: 'foo: a bar: b | c | self first. self second. a + b + c'.
	self deny: tree lastIsReturn.
	self deny: (tree body statements at: 2) isUsed.
	self assert: tree body statements last arguments first isUsed.
	self assert: (tree isLast: tree body statements last).
	self deny: (tree isLast: tree body statements first).
	self assert: (tree defines: 'a').
	self deny: (tree defines: 'c').
	self assert: (tree body defines: 'c').
	self deny: (tree body defines: 'a').
	tree
		addReturn;
		selector: #bar:foo:.
	(tree body)
		addTemporaryNamed: 'd';
		removeTemporaryNamed: 'c'.
	self compare: tree
		to: (RBParser 
				parseMethod: 'bar: a foo: b | d | self first. self second. ^a + b + c').
	self 
		assert: ((tree argumentNames asSet)
				removeAll: #('a' 'b');
				yourself) isEmpty.
	self 
		assert: ((tree allDefinedVariables asSet)
				removeAll: #('a' 'b' 'd');
				yourself) isEmpty.
	tree := RBParser parseExpression: 'self foo: 0'.
	tree selector: #+.
	self compare: tree to: (RBParser parseExpression: 'self + 0').
	self should: [tree selector: #foo] raise: TestResult error.
]

{ #category : 'tests' }
RBParserTest >> testMultimatch [
	| rewriter count |
	count := 0.
	rewriter := RBParseTreeRewriter new.
	rewriter 
		replace: '``@object at: ``@foo'
		with: '``@object foo: ``@foo'
		when: [:aNode | (count := count + 1) == 2].
	self compare: (rewriter
				executeTree: (RBParser parseExpression: 'self at: (bar at: 3)');
				tree)
		to: (RBParser parseExpression: 'self at: (bar foo: 3)')
]

{ #category : 'tests' }
RBParserTest >> testNodesDo [
	| size |
	size := 0.
	self treeWithEverything nodesDo: [:e | size := size + 1].
	self assert: size = 19
]

{ #category : 'tests' }
RBParserTest >> testNumberParsing [
  | numbers node |
  numbers := #(#('1' 1) #('-1' -1) #('123' 123) #('123' 123) #('-123' -123) #('1.1' 1.1) #('-1.1' -1.1) #('1.23' 1.23) #('-1.23' -1.23) #('1e3' 1e3) #('1d3' 1d3) #('-1e3' -1e3) #('1e-3' 1e-3) #('-1e-3' -1e-3) #('0.50s2' 0.50s2) #('0.500s3' 0.500s3) #('0.050s3' 0.050s3)).	"('1q3' 1q3) "	"('2r1e8'  2r1e8) ('-2r1e8' -2r1e8) ('2r1E-8' 2r1e-8) ('-2r1e-8' -2r1e-8) "
  numbers
    do: [ :spec | 
      node := RBParser parseExpression: spec first.
      self assert: node token source = spec first.
      self assert: node value = spec second ]
]

{ #category : 'tests' }
RBParserTest >> testNumberRadixParsing [
	2 to: 32 do: [ :radix | 
		| radixString |
		radixString := radix printString, 'r'.
		0 to: 72 do: [ :i | 
			| x |
			self  assert: (x := ((RBParser parseExpression: (radixString, (i printStringRadix: radix)))
				value)) = i ] ]
]

{ #category : 'tests' }
RBParserTest >> testParents [
	(Array with: self treeWithEverything with: self treeWithReallyEverything) do: [ :tree |
		(Array with: tree with: tree copy) do: [ :root |
			root nodesDo: [ :node |
				node children do: [ :each |
					(each parent isMessage and: [ each parent isCascaded ]) ifFalse: [ 
						self assert: each parent == node.
						self assert: each methodNode == root ] ] ] ] ]
]

{ #category : 'tests' }
RBParserTest >> testParserErrors [
	#(#('self foo. + 3' 11) #('#(' 3) #('self 0' 6) #('self asdf;;asfd' 11)) 
		do: [:each | self parseError: each]
]

{ #category : 'tests' }
RBParserTest >> testParsingLiteralMessages [
	self assert: (RBParser parseExpression: 'nil self nil') isMessage.
	self assert: (RBParser parseExpression: 'self true') isMessage.
	self assert: (RBParser parseExpression: 'self false') isMessage.
	self assert: (RBParser parseExpression: 'self -1') isMessage.
	
	self assert: (RBParser parseMethod: 'nil') isMethod.
	self assert: (RBParser parseMethod: 'true') isMethod.
	self assert: (RBParser parseMethod: 'false') isMethod
]

{ #category : 'tests' }
RBParserTest >> testPatternCascade [
	| rewriter |
	rewriter := RBParseTreeRewriter new.
	rewriter replace: 'self `;messages; foo: 4; `;messages1'
		with: 'self `;messages1; bar: 4; `;messages'.
	self compare: (rewriter
				executeTree: (RBParser 
							parseExpression: 'self foo; printString; foo: 4; bar. self foo: 4');
				tree)
		to: (RBParser 
				parseExpression: 'self bar; bar: 4; foo; printString. self foo:4')
]

{ #category : 'tests' }
RBParserTest >> testPositions [
	| blockNode |
	blockNode := RBParser parseExpression: '[:a :b | ]'.
	self assert: blockNode left = 1.
	self assert: blockNode right = 10.
	self assert: blockNode bar = 8.
	self assert: blockNode sourceInterval = (1 to: 10).
	self assert: blockNode size = 1.	"test dummy collection protocol"
	blockNode printString.	"coverage"
	self deny: (blockNode isLast: (RBVariableNode named: 'b')).
	self compare: blockNode
		to: (RBBlockNode 
				arguments: (OrderedCollection with: (RBVariableNode named: 'a')
						with: (RBVariableNode named: 'b'))
				body: (RBSequenceNode statements: OrderedCollection new)).
]

{ #category : 'tests' }
RBParserTest >> testPragmas [
	| tree node |
	#( ('foo <foo>' #foo ()) ('foo <foo: 1>' #foo: (1)) ('foo <foo: 1.2>' #foo: (1.2)) ('foo <foo: -3>' #foo: (-3)) ('foo <foo: a>' #foo: (a)) ('foo <foo: #a>' #foo: (a)) ('foo <foo: $a>' #foo: ($a)) ('foo <foo: ''bar''>' #foo: ('bar')) ('foo <foo: true>' #foo: (true)) ('foo <foo: false>' #foo: (false)) ('foo <foo: nil>' #foo: (nil)) ('foo <foo: #(1 2)>' #foo: ((1 2))) ('foo <foo: 1 bar: 2>' #foo:bar: (1 2))) do: [ :each |
		tree := RBParser parseMethod: each first.
		self assert: (tree pragmas size = 1).
		node := tree pragmas first.
		self assert: node selector = each second.
		1 to: node arguments size do: [ :i |
			self assert: (node arguments at: i) value = (each last at: i) ].
		self assert: (each first at: node start) = $<.
		self assert: (each first at: node stop) = $> ]
]

{ #category : 'tests' }
RBParserTest >> testPrimitives [
  self assert: (Object parseTreeFor: #'_basicAt:') isPrimitive.
  #(#('foo ^true' false) #('foo <some: tag> ^true' false) #(' foo <some: tag> <primitive: 123> ^true' true))
    do: [ :each | self assert: (RBParser parseMethod: each first) isPrimitive = each last ]
]

{ #category : 'tests' }
RBParserTest >> testProtectedPrimitives [
  self assert: (GemStoneParameters parseTreeFor: #'_getArg:key:') isPrimitive.
  #(#('foo ^true' false) #('foo <some: tag> ^true' false) #(' foo <some: tag> <protected primitive: 123> ^true' true))
    do: [ :each | self assert: (RBParser parseMethod: each first) isPrimitive = each last ]
]

{ #category : 'tests' }
RBParserTest >> testQuerying [
	| tree aNode arg1Node bNode |
	tree := RBParser 
				parseMethod: ('test: a`	| b |`	b := (self foo: a; bar) baz.`	b := super test: b.`	^[:arg1 | self foa1 + (super foo: arg1 foo: a foo: b)]' 
						copyReplaceAll: '`'
						with: (String with: (Character codePoint: 13))).
	self 
		assert: tree selfMessages asSortedCollection asArray = #(#bar #foa1 #foo:).
	self assert: tree superMessages asSortedCollection asArray 
				= #(#foo:foo:foo: #test:).
	aNode := tree whichNodeIsContainedBy: (112 to: 112).
	self assert: aNode name = 'a'.
	bNode := tree whichNodeIsContainedBy: (119 to: 119).
	self assert: bNode name = 'b'.
	arg1Node := tree whichNodeIsContainedBy: (102 to: 105).
	self assert: arg1Node name = 'arg1'.
	self assert: (arg1Node statementNode isMessage 
				and: [arg1Node statementNode selector = #+]).
	self assert: (arg1Node whoDefines: 'arg1') isBlock.
	self assert: (aNode whoDefines: 'a') isMethod.
	self assert: (aNode whoDefines: 'b') isSequence.
	self assert: (tree whichNodeIsContainedBy: (91 to: 119)) selector 
				= #foo:foo:foo:.
	self assert: (tree whichNodeIsContainedBy: (69 to: 121)) isBlock.
	self assert: (tree whichNodeIsContainedBy: (69 to: 118)) isNil.
	self assert: aNode blockVariables asSortedCollection asArray = #('arg1').
	self assert: aNode temporaryVariables asSortedCollection asArray = #('b').
	self assert: tree allDefinedVariables asSortedCollection asArray 
				= #('a' 'arg1' 'b').
	self assert: tree allArgumentVariables asSortedCollection asArray 
				= #('a' 'arg1').
	self 
		assert: tree allTemporaryVariables asSortedCollection asArray = #('b')
]

{ #category : 'tests' }
RBParserTest >> testReadBeforeWritten [
	#(('a ifTrue: [^self]' true ) ('self foo. a := b' false ) ('condition ifTrue: [a := b] ifFalse: [self foo: a]' true ) ('condition ifTrue: [a := b] ifFalse: [self foo]. a isNil' true ) ('condition ifTrue: [a := b]. a := c' false ) ('[a := b] whileFalse: [a isNil]' false ) ('self foo: b' false ) ) do: 
		[:each | 
		self assert: ((RBReadBeforeWrittenTester readBeforeWritten: #('a' ) in: (RBParser parseExpression: each first))
				includes: 'a')
				== each last.
		self assert: (RBReadBeforeWrittenTester isVariable: 'a' readBeforeWrittenIn: (RBParser parseExpression: each first))
				= each last].
	#('| temp read written written1 |
			read ifTrue: [^self].
			written1 := self foo ifFalse: [written := true] ifTrue: [written := false].
			[temp := true] whileTrue: [temp notNil & written].
			^temp' '| read written |
			self foo ifTrue: [written := true] ifFalse: [written := false].
			self foo ifTrue: [read := true].
			^read' '| read written |
			self foo do: [:i | i].
			[| i | i := 1. i == 1] whileFalse: [read notNil]' '| written |
			[written := 2] whileFalse.
			self do: [:each | | read | each & read]' '| read |
			self do: [:each | read := each].
			self do: [:each | each & read]' ) do: 
		[:each | 
		| read | 
		read := RBReadBeforeWrittenTester variablesReadBeforeWrittenIn: (RBParser parseExpression: each).
		self assert: (read size = 1 and: [read includes: 'read'])]
]

{ #category : 'tests' }
RBParserTest >> testReadBeforeWritten1 [
	self 
		assert: (RBReadBeforeWrittenTester variablesReadBeforeWrittenIn: (RBParser 
						parseMethod: 'addAll: aCollection 
	"Answer aCollection, having added all elements
	 of aCollection to the receiver.

	 Fail if aCollection is not a kind of
	 Collection."

	| newSize elementsSize growSize |
	(newSize := aCollection size * 2) > elements size ifTrue: [self rehash: newSize].
	elementsSize := elements size.
	growSize := elementsSize // 2.
	aCollection do: 
			[:newObject | 
			| hashIndex element |
			newObject == nil ifFalse: 
					[hashIndex := self hashIndexFor: newObject.
					
					[(element := elements at: hashIndex) == nil
						ifTrue: 
							[elements at: hashIndex put: newObject.
							(elementCount := elementCount + 1) > growSize ifTrue: 
									[self expand.
									elementsSize := elements size.
									growSize := elementsSize // 2].
							true]
						ifFalse: [element == newObject]]
							whileFalse: 
								[(hashIndex := hashIndex + 1) > elementsSize ifTrue: [hashIndex := 1]]]].
	^aCollection')) 
				isEmpty
]

{ #category : 'tests' }
RBParserTest >> testReplacingNodes [
	| tree search block |
	tree := RBParser 
				parseMethod: '+ a | a b | self ifTrue: [a] ifFalse: [b := c]. a := b. [:b :c :a | a foo: a; foo1: a; foo2: a foo: b]. {a. b}. ^a'.
	search := RBParseTreeSearcher new.
	block := [:aNode :answer | aNode replaceWith: (RBVariableNode named: 'q')].
	search
		matches: 'a' do: block;
		matchesArgument: 'a' do: block.
	search executeTree: tree.
	self 
		assert: tree = (RBParser 
						parseMethod: '+ q | q b | self ifTrue: [q] ifFalse: [b := c]. q := b. [:b :c :q | q foo: q; foo1: q; foo2: q foo: b]. {q. b}. ^q').
	self 
		assert: tree removeDeadCode = (RBParser 
						parseMethod: '+ q | q b | self ifTrue: [] ifFalse: [b := c]. q := b. {q. b}. ^q')
]

{ #category : 'tests' }
RBParserTest >> testRewriteMethods [
	"#('source' 'target' 'source pattern' 'target pattern')"
	#(#('arg1: a arg2: b | temp1 temp2 | self stmt1 ifTrue: [^a]. self arg1: a arg2: b' 'arg2: a arg1: b | temp1 temp2 | self stmt1 ifTrue: [^a]. self arg2: b arg2: a' '`arg1: `var1 `arg2: `var2 | `@temps | ``@.stmts. self `arg1: `var1 `arg2: `var2. `@.stmts1' '`arg2: `var1 `arg1: `var2 | `@temps | ``@.stmts. self `arg2: `var2 `arg2: `var1. `@.stmts1') #('arg1: a arg2: b | temp1 temp2 | self stmt1. self arg1: a arg2: b' 'arg1: a arg2: b | temp1 temp2 | [self stmt1] repeat' '`@args: `@vars | `@temps | `@.stmts. self `@args: `@vars' '`@args: `@vars | `@temps | [`@.stmts] repeat') #('+ a | temps | ^self primitiveValue' '- a | temps | ^self primitiveValue' '+ `temp | `@tmps | `@.stmts' '- `temp | `@tmps | `@.stmts') #('a self stmt1. self stmt2' 'a self stmt1. self stmt2' 'b | `@temps | `@.stmts' 'c | `@temps | `@.stmts') #('a <foo: 1 bar: 2>' 'a <bar: 2 foo: 1>' 'a <`sel1: `#arg1 `sel2: `#arg2>' 'a <`sel2: `#arg2 `sel1: `#arg1>') #('a <foo> self foo' 'b <foo> self foo' 'a `@.stmts' 'b `@.stmts'))
		do: 
			[:each | 
			| rewrite |
			rewrite := RBParseTreeRewriter new.
			rewrite replaceMethod: (each at: 3) with: each last.
			self compare: (RBParser 
						parseMethod: (rewrite
								executeTree: (RBParser parseMethod: each first);
								tree) formattedCode)
				to: (RBParser parseMethod: (each at: 2)).
			rewrite := RBParseTreeRewriter new.
			rewrite replaceTree: (RBParser parseRewriteMethod: (each at: 3))
				withTree: (RBParser parseRewriteMethod: each last).
			self compare: (RBParser 
						parseMethod: (rewrite
								executeTree: (RBParser parseMethod: each first);
								tree) formattedCode)
				to: (RBParser parseMethod: (each at: 2))]
]

{ #category : 'tests' }
RBParserTest >> testRewrites [
	"#('source' 'target' 'source pattern' 'target pattern')"
	#(('[:c | |a| a foo1; foo2]' '[:c | |a| b foo1; foo2]' 'a' 'b' ) ('self foo: 1. bar foo1 foo: 2. (self foo: a) foo: (b foo: c)' 'self bar: 1. bar foo1 bar: 2. (self bar: a) bar: (b bar: c)' '``@rcvr foo: ``@arg1' '``@rcvr bar: ``@arg1' ) ('3 + 4' '4 + 4' '3' '4' ) ('a := self a' 'b := self a' 'a' 'b' ) ('^self at: 1 put: 2' '^self put: 1 put: 2' '^`@rcvr `at: `@arg1 put: `@arg2' '^`@rcvr put: `@arg1 put: `@arg2' ) ('1 + 2 + 3' '0 + 0 + 0' '`#literal' '0' ) ('1 + 2 + 3. 3 foo: 4' '3 + (2 + 1). 4 foo: 3' '``@rcvr `msg: ``@arg' '``@arg `msg: ``@rcvr' ) ('self foo: a bar: b. 1 foo: a bar: b' '2 foo: a bar: b. 1 foo: a bar: b' 'self `@msg: `@args' '2 `@msg: `@args' ) ('a := b. a := c + d' 'b := a. a := c + d' '`var1 := `var2' '`var2 := `var1' ) ('^self foo value: 1' 'self return: (self foo value: 1)' '^`@anything' 'self return: `@anything' ) ('self first; second. self first; second. self a. self b' '2 timesRepeat: [self first; second]. self a. self b' '`.Stmt1. `.Stmt1. `@.stmts' '2 timesRepeat: [`.Stmt1]. `@.stmts' ) ('[:a | self a: 1 c: 2; b]' '[:a | self d: 2 e: 1; f. self halt]' '`@rcvr `msg1: `@arg1 `msg2: `@arg2; `msg' '`@rcvr d: `@arg2 e: `@arg1; f. self halt' )  ) do: 
		[:each | 
		| rewrite | 
		rewrite := RBParseTreeRewriter new.
		rewrite replace: (each at: 3)
			with: each last.
		self compare: (RBParser parseExpression: (rewrite executeTree: (RBParser parseExpression: each first);
				 tree) formattedCode)
			to: (RBParser parseExpression: (each at: 2))]
]

{ #category : 'tests' }
RBParserTest >> testSearching [
	| searcher |
	searcher := RBParseTreeSearcher new.
	searcher matches: '``@rcv at: ``@arg `sel: ``@arg1'
		do: [:aNode :answer | answer + 1].
	self 
		assert: (searcher executeTree: (RBParser 
						parseExpression: 'self at: 1 put: 2; at: 2 ifAbsent: []; ifAbsent: 2 at: 1; at: 4; foo')
				initialAnswer: 0) = 2.
	searcher := RBParseTreeSearcher new.
	searcher matches: '``@rcv `at: ``@arg1 `at: ``@arg2'
		do: [:aNode :answer | answer + 1].
	self 
		assert: (searcher executeTree: (RBParser 
						parseExpression: 'self at: 1 at: 3; at: 1 put: 32; at: 2; foo; at: 1 at: 1 at: 2')
				initialAnswer: 0) = 1.
	searcher := RBParseTreeSearcher new.
	searcher matchesMethod: 'at: `object `put: `o1 ``@rcv `put: 1'
		do: [:aNode :answer | true].
	self assert: (searcher 
				executeTree: (RBParser parseMethod: 'at: a put: b self foo put: 1')
				initialAnswer: false)
]

{ #category : 'tests' }
RBParserTest >> testStatements [
	| tree |
	#(('' 0 0) ('.' 0 1) ('| bar |' 0 0) ('| bar | .' 0 1) ('| bar | ..' 0 2) ('foo. bar' 2 1) ('foo. bar.' 2 2) ('foo. bar. .' 2 3) ('. foo. bar' 2 2)) do: [ :each |
		tree := RBParser parseExpression: each first.
		self assert: tree statements size = each second.
		self assert: tree periods size = each last ]
]

{ #category : 'tests' }
RBParserTest >> testSymbolLiteral [
	| tree |
	#(('# foo' #foo) ('#"bar"foo' #foo) ('##foo' #foo) ('###foo' #foo) ('#foo:' #foo:) ('#foo::' #'foo::') ('#foo::bar' #'foo::bar') ('#foo::bar:' #'foo::bar:') ('#foo::bar::' #'foo::bar::')) do: [ :pair |
		tree := RBParser parseExpression: pair first.
		self assert: tree value = pair second.		
		self assert: tree start = 1.
		self assert: tree stop = pair first size ].
	#(('#1' 1) ('#12' 12) ('#12.3' 12.3) ('# 1' 1) ('##1' 1) ('#"bar"1' 1)) do: [ :pair | 
		tree := RBParser parseExpression: pair first.
		self assert: tree value = pair second.		
		self assert: tree start > 1.
		self assert: tree stop = pair first size ]
]

{ #category : 'tests' }
RBParserTest >> testSymbolNumber [
	| tree |
	#(('#1' 1) ('#12' 12) ('#12.3' 12.3) ('# 1' 1) ('##1' 1) ('#"bar"1' 1)) do: [ :pair | 
		tree := RBParser parseExpression: pair first.
		self assert: tree value = pair second.		
		self assert: tree start > 1.
		self assert: tree stop = pair first size ]
]

{ #category : 'private' }
RBParserTest >> treeWithEverything [
	^ RBParser parseMethod: 'method: arg1 | temps | temps := #(10). temps foo; foo. ^(temps collect: [:e | ])'
]

{ #category : 'private' }
RBParserTest >> treeWithReallyEverything [
	^ RBParser parseMethod: 'method: arg1 <foo> <foo: 1.2> | temps | temps := #[ 1 2 3 ]. temps := #(true false nil 1 1.2 $a foo #foo ''foo'' #() #(1 2) #[] #[1 2]). { 1 negated. 1 + 2. 1 raisedTo: 2 }. temps foo; foo: self. ^ (temps collect: [:e | | btemps | ((e isNil)) ])'
]
