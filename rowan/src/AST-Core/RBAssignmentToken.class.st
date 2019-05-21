"
RBAssignmentToken is the first-class representation of the assignment token ':='

"
Class {
	#name : 'RBAssignmentToken',
	#superclass : 'RBToken',
	#category : 'AST-Core'
}

{ #category : 'testing' }
RBAssignmentToken >> isAssignment [
	^true
]

{ #category : 'private' }
RBAssignmentToken >> length [
	^2
]
