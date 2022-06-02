Class {
	#name : 'RBWorkspaceNode',
	#superclass : 'RBProgramNode',
	#instVars : [
		'body',
		'source'
	],
	#category : 'AST-Core'
}

{ #category : 'comparing' }
RBWorkspaceNode >> = anObject [
	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	self comments = anObject comments
		ifFalse: [ ^ false ].
	self body = anObject body
		ifFalse: [ ^ false ].
	^ true
]

{ #category : 'visitor' }
RBWorkspaceNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor acceptWorkspaceNode: self
]

{ #category : 'accessing' }
RBWorkspaceNode >> addNode: aNode [
	^ body addNode: aNode
]

{ #category : 'accessing' }
RBWorkspaceNode >> body [
	body ifNil: [ body := OrderedCollection new ].
	^ body
]

{ #category : 'accessing' }
RBWorkspaceNode >> body: stmtsNode [
	body := stmtsNode.
	body parent: self
]

{ #category : 'accessing' }
RBWorkspaceNode >> children [
	^ OrderedCollection new
		add: self body;
		yourself
]

{ #category : 'comparing' }
RBWorkspaceNode >> hash [
	^ (self hashForCollection: self comments) bitXor: self body hash
]

{ #category : 'testing' }
RBWorkspaceNode >> isWorkspace [
	^ true
]

{ #category : 'accessing' }
RBWorkspaceNode >> methodNode [
	^ self
]

{ #category : 'copying' }
RBWorkspaceNode >> postCopy [
	super postCopy.
	self body: self body copy
]

{ #category : 'printing' }
RBWorkspaceNode >> printOn: aStream [
	aStream nextPutAll: self formattedCode
]

{ #category : 'accessing' }
RBWorkspaceNode >> source [
	^ source
]

{ #category : 'accessing' }
RBWorkspaceNode >> source: anObject [
	source := anObject
]

{ #category : 'accessing' }
RBWorkspaceNode >> start [
	^ 1
]

{ #category : 'accessing' }
RBWorkspaceNode >> stop [
	^ source size
]
