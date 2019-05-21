"
RBProgramNodeVisitor is an abstract visitor for the RBProgramNodes.


"
Class {
	#name : 'RBProgramNodeVisitor',
	#superclass : 'Object',
	#category : 'AST-Core'
}

{ #category : 'instance creation' }
RBProgramNodeVisitor class >> new [

	^self basicNew initialize
]

{ #category : 'visitor-double dispatching' }
RBProgramNodeVisitor >> acceptArrayNode: anArrayNode [ 
	anArrayNode children do: [:each | self visitNode: each]
]

{ #category : 'visitor-double dispatching' }
RBProgramNodeVisitor >> acceptAssignmentNode: anAssignmentNode [ 
	self visitNode: anAssignmentNode variable.
	self visitNode: anAssignmentNode value
]

{ #category : 'visitor-double dispatching' }
RBProgramNodeVisitor >> acceptBlockNode: aBlockNode [ 
	self visitArguments: aBlockNode arguments.
	self visitNode: aBlockNode body
]

{ #category : 'visitor-double dispatching' }
RBProgramNodeVisitor >> acceptCascadeNode: aCascadeNode [ 
	aCascadeNode messages do: [:each | self visitNode: each]
]

{ #category : 'visitor-double dispatching' }
RBProgramNodeVisitor >> acceptLiteralArrayNode: aRBLiteralArrayNode [ 
	aRBLiteralArrayNode contents do: [:each | self visitNode: each]
]

{ #category : 'visitor-double dispatching' }
RBProgramNodeVisitor >> acceptLiteralNode: aLiteralNode [
]

{ #category : 'visitor-double dispatching' }
RBProgramNodeVisitor >> acceptMessageNode: aMessageNode [ 
	(aMessageNode isCascaded not or: [aMessageNode isFirstCascaded]) 
		ifTrue: [self visitNode: aMessageNode receiver].
	aMessageNode arguments do: [:each | self visitNode: each]
]

{ #category : 'visitor-double dispatching' }
RBProgramNodeVisitor >> acceptMethodNode: aMethodNode [ 
	self visitArguments: aMethodNode arguments.
	aMethodNode pragmas
		do: [ :each | self visitNode: each ].
	self visitNode: aMethodNode body
]

{ #category : 'visitor-double dispatching' }
RBProgramNodeVisitor >> acceptPatternBlockNode: aRBPatternBlockNode [ 
	self visitArguments: aRBPatternBlockNode arguments.
	self visitNode: aRBPatternBlockNode body
]

{ #category : 'visitor-double dispatching' }
RBProgramNodeVisitor >> acceptPatternWrapperBlockNode: aRBPatternWrapperBlockNode [ 
	self visitNode: aRBPatternWrapperBlockNode wrappedNode.
	self visitArguments: aRBPatternWrapperBlockNode arguments.
	self visitNode: aRBPatternWrapperBlockNode body
]

{ #category : 'visitor-double dispatching' }
RBProgramNodeVisitor >> acceptPragmaNode: aPragmaNode [
	aPragmaNode arguments do: [ :each | self visitNode: each ]
]

{ #category : 'visitor-double dispatching' }
RBProgramNodeVisitor >> acceptQueryBlockNode: aBlockNode [
  self visitArguments: aBlockNode arguments.
  self visitNode: aBlockNode body
]

{ #category : 'visitor-double dispatching' }
RBProgramNodeVisitor >> acceptReturnNode: aReturnNode [ 
	self visitNode: aReturnNode value
]

{ #category : 'visitor-double dispatching' }
RBProgramNodeVisitor >> acceptSequenceNode: aSequenceNode [ 
	self visitArguments: aSequenceNode temporaries.
	aSequenceNode statements do: [:each | self visitNode: each]
]

{ #category : 'visitor-double dispatching' }
RBProgramNodeVisitor >> acceptVariableNode: aVariableNode [
]

{ #category : 'initialize release' }
RBProgramNodeVisitor >> initialize [
]

{ #category : 'visiting' }
RBProgramNodeVisitor >> visitArgument: each [ 
	"Here to allow subclasses to detect arguments or temporaries."

	^self visitNode: each
]

{ #category : 'visiting' }
RBProgramNodeVisitor >> visitArguments: aNodeCollection [ 
	^aNodeCollection do: [:each | self visitArgument: each]
]

{ #category : 'visiting' }
RBProgramNodeVisitor >> visitNode: aNode [ 
	^aNode acceptVisitor: self
]
