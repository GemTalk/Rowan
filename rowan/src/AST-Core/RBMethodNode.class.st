"
RBMethodNode is the AST that represents a Smalltalk method.

Instance Variables:
	arguments	<SequenceableCollection of: RBVariableNode>	the arguments to the method
	body	<BRSequenceNode>	the body/statements of the method
	nodeReplacements	<Dictionary>	a dictionary of oldNode -> newNode replacements
	replacements	<Collection of: RBStringReplacement>	the collection of string replacements for each node replacement in the parse tree
	selector	<Symbol | nil>	the method name (cached)
	selectorParts	<SequenceableCollection of: RBValueToken>	the tokens for the selector keywords
	source	<String>	the source we compiled
	tags	<Collection of: Interval>	the source location of any resource/primitive tags


"
Class {
	#name : 'RBMethodNode',
	#superclass : 'RBProgramNode',
	#instVars : [
		'selector',
		'selectorParts',
		'body',
		'source',
		'arguments',
		'pragmas',
		'replacements',
		'nodeReplacements'
	],
	#category : 'AST-Core'
}

{ #category : 'instance creation' }
RBMethodNode class >> selector: aSymbol arguments: variableNodes body: aSequenceNode [ 
	^(self new)
		arguments: variableNodes;
		selector: aSymbol;
		body: aSequenceNode;
		yourself
]

{ #category : 'instance creation' }
RBMethodNode class >> selector: aSymbol body: aSequenceNode [ 
	^self 
		selector: aSymbol
		arguments: #()
		body: aSequenceNode
]

{ #category : 'instance creation' }
RBMethodNode class >> selectorParts: tokenCollection arguments: variableNodes [ 
	^(self new)
		selectorParts: tokenCollection arguments: variableNodes;
		yourself
]

{ #category : 'comparing' }
RBMethodNode >> = anObject [ 
	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	(self selector = anObject selector
		and: [ self pragmas size = anObject pragmas size
		and: [ self body = anObject body ] ])
			ifFalse: [ ^ false ].
	self arguments with: anObject arguments do: [ :first :second |
		first = second 
			ifFalse: [ ^ false ] ].
	self pragmas with: anObject pragmas do: [ :first :second |
		first = second 
			ifFalse: [ ^ false ] ].
	^ true
]

{ #category : 'visitor' }
RBMethodNode >> acceptVisitor: aProgramNodeVisitor [ 
	^aProgramNodeVisitor acceptMethodNode: self
]

{ #category : 'accessing' }
RBMethodNode >> addNode: aNode [
	^ body addNode: aNode
]

{ #category : 'replacing' }
RBMethodNode >> addReplacement: aStringReplacement [ 
	replacements isNil ifTrue: [^self].
	replacements add: aStringReplacement
]

{ #category : 'accessing' }
RBMethodNode >> addReturn [
	^ body addReturn
]

{ #category : 'accessing' }
RBMethodNode >> addSelfReturn [
	^ body addSelfReturn
]

{ #category : 'accessing' }
RBMethodNode >> allArgumentVariables [
	^(self argumentNames asOrderedCollection)
		addAll: super allArgumentVariables;
		yourself
]

{ #category : 'accessing' }
RBMethodNode >> allDefinedVariables [
	^(self argumentNames asOrderedCollection)
		addAll: super allDefinedVariables;
		yourself
]

{ #category : 'accessing' }
RBMethodNode >> argumentNames [
	^self arguments collect: [:each | each name]
]

{ #category : 'accessing' }
RBMethodNode >> arguments [
	^arguments
]

{ #category : 'accessing' }
RBMethodNode >> arguments: variableNodes [ 
	arguments := variableNodes.
	arguments do: [:each | each parent: self]
]

{ #category : 'accessing' }
RBMethodNode >> body [
	^body
]

{ #category : 'accessing' }
RBMethodNode >> body: stmtsNode [ 
	body := stmtsNode.
	body parent: self
]

{ #category : 'private' }
RBMethodNode >> buildSelector [
	| selectorStream |
	selectorStream := WriteStreamPortable on: String new.
	selectorParts do: [:each | selectorStream nextPutAll: each value].
	^selectorStream contents asSymbol
]

{ #category : 'private-replacing' }
RBMethodNode >> changeSourceSelectors: oldSelectorParts arguments: oldArguments [
	"If this is the same number of arguments, we try a one to one replacement of selector parts and arguments. If this is not the case try to rewrite the signature as a whole, what unfortunately drops the comments within the signature."
	
	[ (oldSelectorParts size = selectorParts size and: [ oldArguments size = arguments size ])
		ifTrue: [
			oldSelectorParts with: selectorParts do: [ :old :new | 
				self addReplacement: (RBStringReplacement 
					replaceFrom: old start
					to: old stop
					with: new value) ].
			oldArguments with: arguments do: [ :old :new | 
				self addReplacement: (RBStringReplacement 
					replaceFrom: old start
					to: old stop
					with: new name) ] ]
		ifFalse: [
			self addReplacement: (RBStringReplacement
				replaceFrom: oldSelectorParts first start
				to: (oldArguments notEmpty
					ifTrue: [ oldArguments last stop ]
					ifFalse: [ oldSelectorParts last stop ])
				with: (String streamContents: [ :stream |
					selectorParts keysAndValuesDo: [ :index :part |
						index = 1 ifFalse: [ stream space ].
						stream nextPutAll: part value.
						index <= arguments size
							ifTrue: [ stream space; nextPutAll: (arguments at: index) name ] ] ])) ] ]
		on: Error
		do: [ :ex | ex return ]
]

{ #category : 'accessing' }
RBMethodNode >> children [
	^ OrderedCollection new
		addAll: self arguments;
		addAll: self pragmas;
		add: self body;
		yourself
]

{ #category : 'replacing' }
RBMethodNode >> clearReplacements [
	replacements := nil
]

{ #category : 'matching' }
RBMethodNode >> copyInContext: aDictionary [
	^ self class new
		selectorParts: (self selectorParts collect: [ :each | each copy removePositions ]);
		arguments: (self arguments collect: [ :each | each copyInContext: aDictionary ]);
		pragmas: (self pragmas isEmpty
			ifTrue: [ aDictionary at: '-pragmas-' ifAbsent: [ #() ] ]
			ifFalse: [ self copyList: self pragmas inContext: aDictionary ]);
		body: (self body copyInContext: aDictionary);
		source: (aDictionary at: '-source-' ifAbsentPut: [ self source ]);
		yourself
]

{ #category : 'testing' }
RBMethodNode >> defines: aName [ 
	^arguments anySatisfy: [:each | each name = aName]
]

{ #category : 'comparing' }
RBMethodNode >> equalTo: anObject withMapping: aDictionary [ 
	self class = anObject class ifFalse: [ ^ false ].
	(self selector = anObject selector
		and: [ self pragmas size = anObject pragmas size
		and: [ self body equalTo: anObject body withMapping: aDictionary ] ]) 
			ifFalse: [ ^ false ].
	self arguments with: anObject arguments do: [ :first :second |
		(first equalTo: second	withMapping: aDictionary)
			ifFalse: [ ^ false ].
		aDictionary removeKey: first name ].
	self pragmas with: anObject pragmas do: [ :first :second |
		(first equalTo: second	withMapping: aDictionary) 
			ifFalse: [ ^ false ] ].
	^ true
]

{ #category : 'comparing' }
RBMethodNode >> hash [
	^ ((self selector hash bitXor: (self hashForCollection: self arguments)) bitXor: (self hashForCollection: self pragmas)) bitXor: self body hash
]

{ #category : 'initialize-release' }
RBMethodNode >> initialize [
	replacements := SortedCollection sortBlock: 
					[:a :b | 
					a startPosition < b startPosition 
						or: [a startPosition = b startPosition and: [a stopPosition < b stopPosition]]].
	nodeReplacements := IdentityDictionary new
]

{ #category : 'testing' }
RBMethodNode >> isLast: aNode [ 
	^body isLast: aNode
]

{ #category : 'testing' }
RBMethodNode >> isMethod [
	^true
]

{ #category : 'testing' }
RBMethodNode >> isPrimitive [
	^ self pragmas anySatisfy: [ :each | each isPrimitive ]
]

{ #category : 'testing' }
RBMethodNode >> lastIsReturn [
	^body lastIsReturn
]

{ #category : 'replacing' }
RBMethodNode >> map: oldNode to: newNode [ 
	nodeReplacements at: oldNode put: newNode
]

{ #category : 'replacing' }
RBMethodNode >> mappingFor: oldNode [ 
	^nodeReplacements at: oldNode ifAbsent: [oldNode]
]

{ #category : 'matching' }
RBMethodNode >> match: aNode inContext: aDictionary [ 
	self class = aNode class ifFalse: [ ^ false ].
	aDictionary at: '-source-' put: aNode source.
	self selector = aNode selector ifFalse: [ ^ false ].
	^ (self matchList: arguments against: aNode arguments inContext: aDictionary)
		and: [ (self matchPragmas: self pragmas against: aNode pragmas inContext: aDictionary)
		and: [ body match: aNode body inContext: aDictionary ] ]
]

{ #category : 'matching' }
RBMethodNode >> matchPragmas: matchNodes against: pragmaNodes inContext: aDictionary [
	matchNodes isEmpty ifTrue: [ 
		aDictionary at: '-pragmas-' put: pragmaNodes.
		^ true ].
	^ matchNodes allSatisfy: [ :matchNode |
		pragmaNodes anySatisfy: [ :pragmaNode |
			matchNode match: pragmaNode inContext: aDictionary ] ]
]

{ #category : 'accessing' }
RBMethodNode >> methodComments [
	| methodComments |
	methodComments := OrderedCollection withAll: self comments.
	arguments do: [:each | methodComments addAll: each comments].
	^methodComments asSortedCollection: [:a :b | a first < b first]
]

{ #category : 'accessing' }
RBMethodNode >> methodNode [
	^self
]

{ #category : 'accessing' }
RBMethodNode >> newSource [
	replacements isNil ifTrue: [^self formattedCode].
	^[self reformatSource] on: Error do: [:ex | ex return: self formattedCode]
]

{ #category : 'accessing' }
RBMethodNode >> numArgs [
	^self selector numArgs
]

{ #category : 'copying' }
RBMethodNode >> postCopy [
	super postCopy.
	self arguments: (self arguments collect: [ :each | each copy ]).
	self pragmas: (self pragmas collect: [ :each | each copy ]).
	self body: self body copy
]

{ #category : 'accessing' }
RBMethodNode >> pragmas [
	^ pragmas ifNil: [ #() ]
]

{ #category : 'accessing' }
RBMethodNode >> pragmas: aCollection [
	pragmas := aCollection.
	pragmas do: [ :each | each parent: self ]
]

{ #category : 'accessing' }
RBMethodNode >> primitiveSources [
	^ self pragmas collect: [ :each | self source copyFrom: each first to: each last ]
]

{ #category : 'printing' }
RBMethodNode >> printOn: aStream [ 
	aStream nextPutAll: self formattedCode
]

{ #category : 'testing' }
RBMethodNode >> references: aVariableName [ 
	^body references: aVariableName
]

{ #category : 'private' }
RBMethodNode >> reformatSource [
	| stream newSource newTree |
	stream := WriteStreamPortable on: String new.
	stream
		nextPutAll: (source
			copyFrom: (replacements
				inject: 1
				into: [ :sum :each | 
					stream
						nextPutAll: (source copyFrom: sum to: each startPosition - 1);
						nextPutAll: (each string).
					each stopPosition + 1 ])
			to: source size).
	newSource := stream contents.
	newTree := RBParser 
		parseMethod: newSource 
		onError: [ :msg :pos | ^ self formattedCode ].
	self = newTree
		ifFalse: [ ^ self formattedCode ].
	^ newSource
]

{ #category : 'accessing' }
RBMethodNode >> renameSelector: newSelector andArguments: varNodeCollection [ 
	| oldSelectorParts oldArguments |
	oldSelectorParts := selectorParts.
	oldArguments := arguments.
	self
		arguments: varNodeCollection;
		selector: newSelector.
	self changeSourceSelectors: oldSelectorParts arguments: oldArguments
]

{ #category : 'replacing' }
RBMethodNode >> replaceNode: aNode withNode: anotherNode [ 
	aNode == body ifTrue: [self body: anotherNode].
	self arguments: (arguments 
				collect: [:each | each == aNode ifTrue: [anotherNode] ifFalse: [each]])
]

{ #category : 'accessing' }
RBMethodNode >> selector [
	^selector isNil
		ifTrue: [selector := self buildSelector]
		ifFalse: [selector]
]

{ #category : 'accessing' }
RBMethodNode >> selector: aSelector [ 
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
RBMethodNode >> selectorParts [
	^selectorParts
]

{ #category : 'private' }
RBMethodNode >> selectorParts: tokenCollection [ 
	selectorParts := tokenCollection
]

{ #category : 'initialize-release' }
RBMethodNode >> selectorParts: tokenCollection arguments: variableNodes [ 
	selectorParts := tokenCollection.
	self arguments: variableNodes
]

{ #category : 'accessing' }
RBMethodNode >> source [
	^source
]

{ #category : 'accessing' }
RBMethodNode >> source: anObject [
	source := anObject
]

{ #category : 'accessing' }
RBMethodNode >> start [
	^1
]

{ #category : 'accessing' }
RBMethodNode >> stop [
	^source size
]

{ #category : 'testing' }
RBMethodNode >> uses: aNode [ 
	^body == aNode and: [aNode lastIsReturn]
]
