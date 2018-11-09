"
RBMessageNode is an AST node that represents a message send.

Instance Variables:
	arguments	<SequenceableCollection of: RBValueNode>	our argument nodes
	receiver	<RBValueNode>	the receiver's node
	selector	<Symbol | nil>	the selector we're sending (cached)
	selectorParts	<SequenceableCollection of: RBValueToken>	the tokens for each keyword


"
Class {
	#name : 'RBMessageNode',
	#superclass : 'RBValueNode',
	#instVars : [
		'receiver',
		'selector',
		'selectorParts',
		'arguments'
	],
	#category : 'AST-Core-Nodes'
}

{ #category : 'instance creation' }
RBMessageNode class >> receiver: aValueNode selector: aSymbol [ 
	^self 
		receiver: aValueNode
		selector: aSymbol
		arguments: #()
]

{ #category : 'instance creation' }
RBMessageNode class >> receiver: aValueNode selector: aSymbol arguments: valueNodes [ 
	^(self new)
		receiver: aValueNode;
		arguments: valueNodes;
		selector: aSymbol;
		yourself
]

{ #category : 'instance creation' }
RBMessageNode class >> receiver: aValueNode selectorParts: keywordTokens arguments: valueNodes [ 
	^(self new)
		receiver: aValueNode
			selectorParts: keywordTokens
			arguments: valueNodes;
		yourself
]

{ #category : 'comparing' }
RBMessageNode >> = anObject [ 
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	(self receiver = anObject receiver 
		and: [self selector = anObject selector]) ifFalse: [^false].
	1 to: self arguments size
		do: [:i | (self arguments at: i) = (anObject arguments at: i) ifFalse: [^false]].
	^true
]

{ #category : 'visitor' }
RBMessageNode >> acceptVisitor: aProgramNodeVisitor [ 
	^aProgramNodeVisitor acceptMessageNode: self
]

{ #category : 'accessing' }
RBMessageNode >> arguments [
	^arguments isNil ifTrue: [#()] ifFalse: [arguments]
]

{ #category : 'accessing' }
RBMessageNode >> arguments: argCollection [ 
	arguments := argCollection.
	arguments do: [:each | each parent: self]
]

{ #category : 'querying' }
RBMessageNode >> bestNodeFor: anInterval [ 
	(self intersectsInterval: anInterval) ifFalse: [^nil].
	(self containedBy: anInterval) ifTrue: [^self].
	selectorParts do: 
			[:each | 
			((anInterval first between: each start and: each stop) 
				or: [each start between: anInterval first and: anInterval last]) 
					ifTrue: [^self]].
	self children do: 
			[:each | 
			| node |
			node := each bestNodeFor: anInterval.
			node notNil ifTrue: [^node]]
]

{ #category : 'private' }
RBMessageNode >> buildSelector [
	| selectorStream |
	selectorStream := AnsiWriteStream on: String new.
	selectorParts do: [ :each | selectorStream nextPutAll: each value ].
	^ selectorStream contents asSymbol
]

{ #category : 'accessing' }
RBMessageNode >> children [
	^(OrderedCollection with: self receiver)
		addAll: self arguments;
		yourself
]

{ #category : 'matching' }
RBMessageNode >> copyInContext: aDictionary [ 
	^ self class new
		receiver: (self receiver copyInContext: aDictionary);
		selectorParts: (self selectorParts collect: [ :each | each copy removePositions ]);
		arguments: (self arguments collect: [ :each | each copyInContext: aDictionary ]);
		yourself
]

{ #category : 'comparing' }
RBMessageNode >> equalTo: anObject withMapping: aDictionary [ 
	self class = anObject class ifFalse: [^false].
	((self receiver equalTo: anObject receiver withMapping: aDictionary)
		and: [self selector = anObject selector]) ifFalse: [^false].
	1 to: self arguments size
		do: 
			[:i | 
			((self arguments at: i) equalTo: (anObject arguments at: i)
				withMapping: aDictionary) ifFalse: [^false]].
	^true
]

{ #category : 'comparing' }
RBMessageNode >> hash [
	^ (self receiver hash bitXor: self selector hash) bitXor: (self hashForCollection: self arguments)
]

{ #category : 'testing' }
RBMessageNode >> isBinary [
	^(self isUnary or: [self isKeyword]) not
]

{ #category : 'testing' }
RBMessageNode >> isCascaded [
	^parent notNil and: [parent isCascade]
]

{ #category : 'private-replacing' }
RBMessageNode >> isContainmentReplacement: aNode [ 
	^(self mappingFor: self receiver) = aNode 
		or: [self arguments anySatisfy: [:each | (self mappingFor: each) = aNode]]
]

{ #category : 'testing' }
RBMessageNode >> isFirstCascaded [
	^self isCascaded and: [parent messages first == self]
]

{ #category : 'testing' }
RBMessageNode >> isKeyword [
	^selectorParts first value last = $:
]

{ #category : 'testing' }
RBMessageNode >> isMessage [
	^true
]

{ #category : 'testing' }
RBMessageNode >> isSelfSend [
	^ self receiver isVariable and: [ self receiver name = 'self' ]
]

{ #category : 'testing' }
RBMessageNode >> isSuperSend [
	^ self receiver isVariable and: [ self receiver name = 'super' ]
]

{ #category : 'testing' }
RBMessageNode >> isUnary [
	^arguments isEmpty
]

{ #category : 'testing' }
RBMessageNode >> lastIsReturn [
	^ (#(#ifTrue:ifFalse: #ifFalse:ifTrue: #ifNil:ifNotNil: #ifNotNil:ifNil:) includes: self selector)
		and: [ arguments first isBlock and: [ arguments first body lastIsReturn 
		and: [ arguments last isBlock and: [ arguments last body lastIsReturn ] ] ] ]
]

{ #category : 'matching' }
RBMessageNode >> match: aNode inContext: aDictionary [ 
	aNode class = self class ifFalse: [^false].
	self selector = aNode selector ifFalse: [^false].
	(receiver match: aNode receiver inContext: aDictionary) ifFalse: [^false].
	1 to: arguments size
		do: 
			[:i | 
			((arguments at: i) match: (aNode arguments at: i) inContext: aDictionary)
				ifFalse: [^false]].
	^true
]

{ #category : 'testing' }
RBMessageNode >> needsParenthesis [
	^parent isNil 
		ifTrue: [false]
		ifFalse: 
			[self precedence > parent precedence 
				or: [self precedence = parent precedence and: [self isUnary not]]]
]

{ #category : 'accessing' }
RBMessageNode >> numArgs [
	^self selector numArgs
]

{ #category : 'copying' }
RBMessageNode >> postCopy [
	super postCopy.
	self receiver: self receiver copy.
	self arguments: (self arguments collect: [ :each | each copy ])
]

{ #category : 'accessing' }
RBMessageNode >> precedence [
	^self isUnary 
		ifTrue: [1]
		ifFalse: [self isKeyword ifTrue: [3] ifFalse: [2]]
]

{ #category : 'accessing' }
RBMessageNode >> receiver [
	^receiver
]

{ #category : 'accessing' }
RBMessageNode >> receiver: aValueNode [ 
	receiver := aValueNode.
	receiver parent: self
]

{ #category : 'initialize-release' }
RBMessageNode >> receiver: aValueNode selectorParts: keywordTokens arguments: valueNodes [ 
	self receiver: aValueNode.
	selectorParts := keywordTokens.
	self arguments: valueNodes
]

{ #category : 'accessing' }
RBMessageNode >> renameSelector: newSelector andArguments: varNodeCollection [ 
	self
		arguments: varNodeCollection;
		selector: newSelector
]

{ #category : 'private-replacing' }
RBMessageNode >> replaceContainmentSourceWith: aNode [ 
	| originalNode needsParenthesis |
	needsParenthesis := aNode hasParentheses not and: [aNode needsParenthesis].
	originalNode := (self mappingFor: self receiver) = aNode 
				ifTrue: [self receiver]
				ifFalse: [self arguments detect: [:each | (self mappingFor: each) = aNode]].
	self
		addReplacement: (RBStringReplacement 
					replaceFrom: self start
					to: originalNode start - 1
					with: (needsParenthesis ifTrue: ['('] ifFalse: ['']));
		addReplacement: (RBStringReplacement 
					replaceFrom: originalNode stop + 1
					to: self stop
					with: (needsParenthesis ifTrue: [')'] ifFalse: ['']))
]

{ #category : 'replacing' }
RBMessageNode >> replaceNode: aNode withNode: anotherNode [ 
	"If we're inside a cascade node and are changing the receiver, change all the receivers"

	receiver == aNode 
		ifTrue: 
			[self receiver: anotherNode.
			(parent notNil and: [parent isCascade]) 
				ifTrue: [parent messages do: [:each | each receiver: anotherNode]]].
	self arguments: (arguments 
				collect: [:each | each == aNode ifTrue: [anotherNode] ifFalse: [each]])
]

{ #category : 'private-replacing' }
RBMessageNode >> replaceSourceWith: aNode [ 
	(self isContainmentReplacement: aNode) 
		ifTrue: [^self replaceContainmentSourceWith: aNode].
	aNode isMessage ifFalse: [^super replaceSourceWith: aNode].
	^self replaceSourceWithMessageNode: aNode
]

{ #category : 'private-replacing' }
RBMessageNode >> replaceSourceWithMessageNode: aNode [ 
	| isBinaryToKeyword |
	self numArgs = aNode numArgs ifFalse: [^super replaceSourceWith: aNode].
	self arguments with: aNode arguments
		do: [:old :new | (self mappingFor: old) = new ifFalse: [^super replaceSourceWith: aNode]].
	(self mappingFor: self receiver) = aNode receiver 
		ifFalse:
			[(self receiver isVariable and: [aNode receiver isVariable])
				ifFalse:
					[^super replaceSourceWith: aNode].
			self addReplacement:
				(RBStringReplacement
					replaceFrom: self receiver start
					to: self receiver stop
					with: aNode receiver name)].
	(isBinaryToKeyword := self isBinary and: [aNode isKeyword]) 
		ifTrue: 
			[(self hasParentheses not and: [self parent precedence <= aNode precedence]) 
				ifTrue: 
					[self
						addReplacement: (RBStringReplacement 
									replaceFrom: self start
									to: self start - 1
									with: '(');
						addReplacement: (RBStringReplacement 
									replaceFrom: self stop + 1
									to: self stop
									with: ')')]].
	self selectorParts with: aNode selectorParts
		do: 
			[:old :new | 
			old value ~= new value 
				ifTrue: 
					[self addReplacement: (RBStringReplacement 
								replaceFrom: old start
								to: old stop
								with: ((isBinaryToKeyword 
										and: [(self source at: old start - 1) isSqueakSeparator not]) 
											ifTrue: [' ' , new value]
											ifFalse: [new value]))]]
]

{ #category : 'accessing' }
RBMessageNode >> selector [
	^selector isNil
		ifTrue: [selector := self buildSelector]
		ifFalse: [selector]
]

{ #category : 'accessing' }
RBMessageNode >> selector: aSelector [ 
	| keywords numArgs |
	keywords := aSelector keywords.
	numArgs := aSelector numArgs.
	numArgs == arguments size 
		ifFalse: 
			[self 
				error: 'Attempting to assign selector with wrong number of arguments.'].
	selectorParts := numArgs == 0 
				ifTrue: [Array with: (RBIdentifierToken value: keywords first start: nil)]
				ifFalse: 
					[keywords first last = $: 
						ifTrue: [keywords collect: [:each | RBKeywordToken value: each start: nil]]
						ifFalse: [Array with: (RBBinarySelectorToken value: aSelector start: nil)]].
	selector := aSelector asSymbol
]

{ #category : 'private' }
RBMessageNode >> selectorParts [
	^ selectorParts
]

{ #category : 'private' }
RBMessageNode >> selectorParts: tokenCollection [ 
	selectorParts := tokenCollection
]

{ #category : 'accessing' }
RBMessageNode >> sentMessages [
	^ super sentMessages
		add: self selector;
		yourself
]

{ #category : 'accessing' }
RBMessageNode >> startWithoutParentheses [
	^receiver start
]

{ #category : 'accessing' }
RBMessageNode >> stopWithoutParentheses [
	^arguments isEmpty 
		ifTrue: [selectorParts first stop]
		ifFalse: [arguments last stop]
]
