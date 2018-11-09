"
RBPragmaNode is an AST node that represents a method pragma.

Instance Variables:
	arguments <SequenceableCollection of: RBLiteralNode> our argument nodes
	left <Integer | nil> position of <
	right <Integer | nil> position of >
	selector <Symbol | nil>	the selector we're sending (cached)
	selectorParts <SequenceableCollection of: RBValueToken> the tokens for each keyword
"
Class {
	#name : 'RBPragmaNode',
	#superclass : 'RBProgramNode',
	#instVars : [
		'selector',
		'selectorParts',
		'arguments',
		'isProtected',
		'left',
		'right'
	],
	#category : 'AST-Core-Nodes'
}

{ #category : 'instance creation' }
RBPragmaNode class >> selectorParts: keywordTokens arguments: valueNodes [ 
	^ self new selectorParts: keywordTokens arguments: valueNodes
]

{ #category : 'comparing' }
RBPragmaNode >> = anObject [
  self == anObject
    ifTrue: [ ^ true ].
  self class = anObject class
    ifFalse: [ ^ false ].
  self selector = anObject selector
    ifFalse: [ ^ false ].
  1 to: self arguments size do: [ :i | 
    (self arguments at: i) = (anObject arguments at: i)
      ifFalse: [ ^ false ] ].
  self isProtected == anObject isProtected
    ifFalse: [ ^ false ].
  ^ true
]

{ #category : 'visitor' }
RBPragmaNode >> acceptVisitor: aProgramNodeVisitor [ 
	^ aProgramNodeVisitor acceptPragmaNode: self
]

{ #category : 'accessing' }
RBPragmaNode >> arguments [
	^ arguments ifNil: [ #() ]
]

{ #category : 'accessing' }
RBPragmaNode >> arguments: aLiteralCollection [ 
	arguments := aLiteralCollection.
	arguments do: [ :each | each parent: self ]
]

{ #category : 'private' }
RBPragmaNode >> buildSelector [
	| selectorStream |
	selectorStream := AnsiWriteStream on: String new.
	selectorParts do: [ :each | selectorStream nextPutAll: each value ].
	^ selectorStream contents asSymbol
]

{ #category : 'accessing' }
RBPragmaNode >> children [
	^ self arguments
]

{ #category : 'matching' }
RBPragmaNode >> copyInContext: aDictionary [ 
	^ self class new
		selectorParts: (self selectorParts collect: [ :each | each copy removePositions ]);
		arguments: (self arguments collect: [ :each | each copyInContext: aDictionary ]);
		yourself
]

{ #category : 'comparing' }
RBPragmaNode >> equalTo: anObject withMapping: aDictionary [ 
	self class = anObject class ifFalse: [ ^ false ].
	self selector = anObject selector ifFalse: [ ^ false ].
	self arguments with: anObject arguments do: [ :first :second |
		(first equalTo: second withMapping: aDictionary) 
			ifFalse: [ ^ false ] ].
	^ true
]

{ #category : 'comparing' }
RBPragmaNode >> hash [
	^ self selector hash bitXor: (self hashForCollection: self arguments)
]

{ #category : 'testing' }
RBPragmaNode >> isBinary [
	^ (self isUnary or: [self isKeyword]) not
]

{ #category : 'testing' }
RBPragmaNode >> isKeyword [
	^ selectorParts first value last = $:
]

{ #category : 'testing' }
RBPragmaNode >> isPragma [
	^ true
]

{ #category : 'testing' }
RBPragmaNode >> isPrimitive [
	^ #(primitive: primitive:error: primitive:error:module: primitive:module: primitive:module:error:) includes: self selector
]

{ #category : 'testing' }
RBPragmaNode >> isProtected [
  isProtected ifNil: [ ^ false ].
  ^ isProtected
]

{ #category : 'testing' }
RBPragmaNode >> isProtected: aBool [
  isProtected := aBool
]

{ #category : 'testing' }
RBPragmaNode >> isProtectedPrimitive [
  ^ self isProtected and: [ self isPrimitive ]
]

{ #category : 'testing' }
RBPragmaNode >> isUnary [
	^ arguments isEmpty
]

{ #category : 'accessing-token' }
RBPragmaNode >> left [
	^ left
]

{ #category : 'accessing-token' }
RBPragmaNode >> left: anInteger [
	left := anInteger
]

{ #category : 'matching' }
RBPragmaNode >> match: aNode inContext: aDictionary [ 
	aNode class = self class
		ifFalse: [ ^ false ].
	self selector = aNode selector
		ifFalse: [ ^ false ].
	self arguments with: aNode arguments do: [ :first :second |
		(first match: second inContext: aDictionary)
			ifFalse: [ ^ false ] ].
	^ true
]

{ #category : 'accessing' }
RBPragmaNode >> numArgs [
	^ self selector numArgs
]

{ #category : 'copying' }
RBPragmaNode >> postCopy [
	super postCopy.
	self arguments: (self arguments collect: [ :each | each copy ])
]

{ #category : 'replacing' }
RBPragmaNode >> replaceNode: aNode withNode: anotherNode [ 
	self arguments: (arguments collect: [ :each | 
		each == aNode 
			ifTrue: [ anotherNode ]
			ifFalse: [ each ] ])
]

{ #category : 'accessing-token' }
RBPragmaNode >> right [
	^ right
]

{ #category : 'accessing-token' }
RBPragmaNode >> right: anInteger [
	right := anInteger
]

{ #category : 'accessing' }
RBPragmaNode >> selector [
	^ selector ifNil: [ selector := self buildSelector ]
]

{ #category : 'accessing' }
RBPragmaNode >> selector: aSelector [ 
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
RBPragmaNode >> selectorParts [
	^ selectorParts
]

{ #category : 'private' }
RBPragmaNode >> selectorParts: tokenCollection [ 
	selectorParts := tokenCollection
]

{ #category : 'initialization' }
RBPragmaNode >> selectorParts: keywordTokens arguments: valueNodes [ 
	self selectorParts: keywordTokens.
	self arguments: valueNodes
]

{ #category : 'accessing' }
RBPragmaNode >> sentMessages [
	^ super sentMessages
		add: self selector;
		yourself
]

{ #category : 'accessing' }
RBPragmaNode >> start [
	^ left
]

{ #category : 'accessing' }
RBPragmaNode >> stop [
	^ right
]
