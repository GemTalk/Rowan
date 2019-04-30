Class {
	#name : 'RBPatternPragmaNode',
	#superclass : 'RBPragmaNode',
	#instVars : [
		'isList'
	],
	#category : 'AST-Core'
}

{ #category : 'instance creation' }
RBPatternPragmaNode class >> selectorParts: keywordTokens arguments: valueNodes [ 
	^ (keywordTokens anySatisfy: [ :each | each isPatternVariable ]) 
		ifTrue: [ super selectorParts: keywordTokens arguments: valueNodes ]
		ifFalse: [ RBPragmaNode selectorParts: keywordTokens arguments: valueNodes ]
]

{ #category : 'matching' }
RBPatternPragmaNode >> copyInContext: aDictionary [ 
	| selectors |
	selectors := self isSelectorList 
		ifTrue: [ (aDictionary at: selectorParts first value) keywords ]
		ifFalse: [ self selectorParts collect: [ :each | aDictionary at: each value ] ].
	^ RBPragmaNode new
		selectorParts: (selectors collect: [ :each | 
			(each last = $: ifTrue: [ RBKeywordToken ] ifFalse: [ RBIdentifierToken ]) 
				value: each start: nil ]);
		arguments: (self copyList: self arguments inContext: aDictionary);
		yourself
]

{ #category : 'testing-matching' }
RBPatternPragmaNode >> isPatternNode [
	^ true
]

{ #category : 'testing-matching' }
RBPatternPragmaNode >> isSelectorList [
	^ isList
]

{ #category : 'matching' }
RBPatternPragmaNode >> match: aNode inContext: aDictionary [
	aNode class = self matchingClass
		ifFalse: [ ^ false ].
	self isSelectorList ifTrue: [
		^ (aDictionary at: selectorParts first value ifAbsentPut: [ aNode selector ]) = aNode selector
			and: [ (aDictionary at: arguments first ifAbsentPut: [ aNode arguments ]) = aNode arguments ] ].
	^ self matchArgumentsAgainst: aNode inContext: aDictionary
]

{ #category : 'matching' }
RBPatternPragmaNode >> matchArgumentsAgainst: aNode inContext: aDictionary [ 
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
RBPatternPragmaNode >> matchingClass [
	^ RBPragmaNode
]

{ #category : 'matching' }
RBPatternPragmaNode >> matchSelectorAgainst: aNode inContext: aDictionary [ 
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

{ #category : 'initialization' }
RBPatternPragmaNode >> selectorParts: keywordTokens arguments: valueNodes [ 
	super selectorParts: keywordTokens arguments: valueNodes.
	isList := (keywordTokens first value at: 2) == self listCharacter
]

{ #category : 'accessing' }
RBPatternPragmaNode >> sentMessages [
	^ super sentMessages
		remove: self selector ifAbsent: [ ];
		yourself
]
