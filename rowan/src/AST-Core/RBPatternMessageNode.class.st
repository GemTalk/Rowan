"
RBPatternMessageNode is a RBMessageNode that will match other message nodes without their selectors being equal. 

Instance Variables:
	isCascadeList	<Boolean>	are we matching a list of message nodes in a cascaded message
	isList	<Boolean>	are we matching each keyword or matching all keywords together (e.g., `keyword1: would match a one argument method whereas `@keywords: would match 0 or more arguments)
"
Class {
	#name : 'RBPatternMessageNode',
	#superclass : 'RBMessageNode',
	#instVars : [
		'isList',
		'isCascadeList'
	],
	#category : 'AST-Core-Pattern'
}

{ #category : 'instance creation' }
RBPatternMessageNode class >> receiver: aValueNode selectorParts: keywordTokens arguments: valueNodes [ 
	^(keywordTokens anySatisfy: [:each | each isPatternVariable]) 
		ifTrue: 
			[super 
				receiver: aValueNode
				selectorParts: keywordTokens
				arguments: valueNodes]
		ifFalse: 
			[RBMessageNode 
				receiver: aValueNode
				selectorParts: keywordTokens
				arguments: valueNodes]
]

{ #category : 'matching' }
RBPatternMessageNode >> copyInContext: aDictionary [ 
	| selectors |
	self isList 
		ifTrue: [ ^ aDictionary at: self ].
	selectors := self isSelectorList 
		ifTrue: [ (aDictionary at: selectorParts first value) keywords ]
		ifFalse: [ self selectorParts collect: [ :each | aDictionary at: each value ] ].
	^ RBMessageNode new
		receiver: (self receiver copyInContext: aDictionary);
		selectorParts: (selectors collect: [ :each | 
			(each last = $: ifTrue: [ RBKeywordToken ] ifFalse: [ RBIdentifierToken ]) 
				value: each start: nil ]);
		arguments: (self copyList: self arguments inContext: aDictionary);
		yourself
]

{ #category : 'testing-matching' }
RBPatternMessageNode >> isList [
	^isCascadeList and: [parent notNil and: [parent isCascade]]
]

{ #category : 'testing-matching' }
RBPatternMessageNode >> isPatternNode [
	^true
]

{ #category : 'testing-matching' }
RBPatternMessageNode >> isSelectorList [
	^isList
]

{ #category : 'matching' }
RBPatternMessageNode >> match: aNode inContext: aDictionary [ 
	aNode class == self matchingClass ifFalse: [^false].
	(receiver match: aNode receiver inContext: aDictionary) ifFalse: [^false].
	self isSelectorList 
		ifTrue: 
			[^(aDictionary at: selectorParts first value ifAbsentPut: [aNode selector]) 
				== aNode selector and: 
						[(aDictionary at: arguments first ifAbsentPut: [aNode arguments]) 
							= aNode arguments]].
	^self matchArgumentsAgainst: aNode inContext: aDictionary
]

{ #category : 'matching' }
RBPatternMessageNode >> matchArgumentsAgainst: aNode inContext: aDictionary [ 
	self arguments size = aNode arguments size
		ifFalse: [ ^ false ].
	(self matchSelectorAgainst: aNode inContext: aDictionary) 
		ifFalse: [ ^ false ].
	self arguments with: aNode arguments do: [ :first :second |
		(first match: second inContext: aDictionary) 
			ifFalse: [ ^ false ] ].
	^ true
]

{ #category : 'private' }
RBPatternMessageNode >> matchingClass [
	^RBMessageNode
]

{ #category : 'matching' }
RBPatternMessageNode >> matchSelectorAgainst: aNode inContext: aDictionary [ 
	self selectorParts with: aNode selectorParts do: [ :first :second |
		| keyword |
		keyword := aDictionary
			at: first value
			ifAbsentPut: [ 
				first isPatternVariable 
					ifTrue: [ second value ]
					ifFalse: [ first value ] ].
		keyword = second value 
			ifFalse: [ ^ false ] ].
	^ true
]

{ #category : 'initialize-release' }
RBPatternMessageNode >> receiver: aValueNode selectorParts: keywordTokens arguments: valueNodes [ 
	| message |
	super 
		receiver: aValueNode
		selectorParts: keywordTokens
		arguments: valueNodes.
	isCascadeList := isList := false.
	message := keywordTokens first value.
	2 to: message size
		do: 
			[:i | 
			| character |
			character := message at: i.
			character == self listCharacter 
				ifTrue: [isList := true]
				ifFalse: 
					[character == self cascadeListCharacter 
						ifTrue: [isCascadeList := true]
						ifFalse: [^self]]]
]

{ #category : 'accessing' }
RBPatternMessageNode >> sentMessages [
	^ super sentMessages
		remove: self selector ifAbsent: [ ];
		yourself
]
