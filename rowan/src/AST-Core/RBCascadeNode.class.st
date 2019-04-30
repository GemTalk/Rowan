"
RBCascadeNode is an AST node for cascaded messages (e.g., ""self print1 ; print2"").

Instance Variables:
	messages	<SequenceableCollection of: RBMessageNode>	the messages 
	semicolons	<SequenceableCollection of: Integer>	positions of the ; between messages


"
Class {
	#name : 'RBCascadeNode',
	#superclass : 'RBValueNode',
	#instVars : [
		'messages',
		'semicolons'
	],
	#category : 'AST-Core'
}

{ #category : 'instance creation' }
RBCascadeNode class >> messages: messageNodes [ 
	^self new messages: messageNodes
]

{ #category : 'instance creation' }
RBCascadeNode class >> messages: messageNodes semicolons: integerCollection [ 
	^self new messages: messageNodes semicolons: integerCollection
]

{ #category : 'comparing' }
RBCascadeNode >> = anObject [ 
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	self messages size = anObject messages size ifFalse: [^false].
	1 to: self messages size
		do: [:i | (self messages at: i) = (anObject messages at: i) ifFalse: [^false]].
	^true
]

{ #category : 'visitor' }
RBCascadeNode >> acceptVisitor: aProgramNodeVisitor [ 
	^aProgramNodeVisitor acceptCascadeNode: self
]

{ #category : 'querying' }
RBCascadeNode >> bestNodeFor: anInterval [ 
	| selectedChildren |
	(self intersectsInterval: anInterval) ifFalse: [^nil].
	(self containedBy: anInterval) ifTrue: [^self].
	messages 
		reverseDo: [:each | (each containedBy: anInterval) ifTrue: [^each]].
	selectedChildren := (messages 
				collect: [:each | each bestNodeFor: anInterval]) 
					reject: [:each | each isNil].
	^selectedChildren detect: [:each | true] ifNone: [nil]
]

{ #category : 'accessing' }
RBCascadeNode >> children [
	^self messages
]

{ #category : 'matching' }
RBCascadeNode >> copyInContext: aDictionary [ 
	^ self class new
		messages: (self copyList: self messages inContext: aDictionary);
		yourself
]

{ #category : 'testing' }
RBCascadeNode >> directlyUses: aNode [ 
	^messages last = aNode and: [self isDirectlyUsed]
]

{ #category : 'comparing' }
RBCascadeNode >> equalTo: anObject withMapping: aDictionary [ 
	self class = anObject class ifFalse: [^false].
	self messages size = anObject messages size ifFalse: [^false].
	1 to: self messages size
		do: 
			[:i | 
			((self messages at: i) equalTo: (anObject messages at: i)
				withMapping: aDictionary) ifFalse: [^false]].
	^true
]

{ #category : 'comparing' }
RBCascadeNode >> hash [
	^ self hashForCollection: self messages
]

{ #category : 'testing' }
RBCascadeNode >> isCascade [
	^true
]

{ #category : 'matching' }
RBCascadeNode >> match: aNode inContext: aDictionary [ 
	aNode class = self class ifFalse: [^false].
	^self 
		matchList: messages
		against: aNode messages
		inContext: aDictionary
]

{ #category : 'accessing' }
RBCascadeNode >> messages [
	^messages
]

{ #category : 'accessing' }
RBCascadeNode >> messages: messageNodeCollection [ 
	messages := messageNodeCollection.
	messages do: [:each | each parent: self]
]

{ #category : 'initialize-release' }
RBCascadeNode >> messages: messageNodes semicolons: integerCollection [ 
	self messages: messageNodes.
	semicolons := integerCollection
]

{ #category : 'testing' }
RBCascadeNode >> needsParenthesis [
	^parent isNil 
		ifTrue: [false]
		ifFalse: [self precedence > parent precedence]
]

{ #category : 'copying' }
RBCascadeNode >> postCopy [
	super postCopy.
	self messages: (self messages collect: [ :each | each copy ])
]

{ #category : 'accessing' }
RBCascadeNode >> precedence [
	^4
]

{ #category : 'accessing' }
RBCascadeNode >> receiver [
	^self messages first receiver
]

{ #category : 'replacing' }
RBCascadeNode >> replaceNode: aNode withNode: anotherNode [ 
	self messages: (messages 
				collect: [:each | each == aNode ifTrue: [anotherNode] ifFalse: [each]])
]

{ #category : 'accessing-token' }
RBCascadeNode >> semicolons [
	^ semicolons
]

{ #category : 'accessing-token' }
RBCascadeNode >> semicolons: anArray [
	semicolons := anArray
]

{ #category : 'accessing' }
RBCascadeNode >> startWithoutParentheses [
	^messages first start
]

{ #category : 'accessing' }
RBCascadeNode >> statementComments [
	| statementComments |
	statementComments := OrderedCollection withAll: self comments.
	statementComments addAll: messages first receiver statementComments.
	messages do: 
			[:each | 
			each arguments 
				do: [:arg | statementComments addAll: arg statementComments]].
	^statementComments asSortedCollection: [:a :b | a first < b first]
]

{ #category : 'accessing' }
RBCascadeNode >> stopWithoutParentheses [
	^messages last stop
]

{ #category : 'testing' }
RBCascadeNode >> uses: aNode [ 
	^messages last = aNode and: [self isUsed]
]

{ #category : 'querying' }
RBCascadeNode >> whichNodeIsContainedBy: anInterval [ 
	| selectedChildren |
	(self intersectsInterval: anInterval) ifFalse: [^nil].
	(self containedBy: anInterval) ifTrue: [^self].
	messages 
		reverseDo: [:each | (each containedBy: anInterval) ifTrue: [^each]].
	selectedChildren := (messages 
				collect: [:each | each whichNodeIsContainedBy: anInterval]) 
					reject: [:each | each isNil].
	^selectedChildren detect: [:each | true] ifNone: [nil]
]
