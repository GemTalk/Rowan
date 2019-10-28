Class {
	#name : 'RBQueryBlockNode',
	#superclass : 'RBBlockNode',
	#category : 'AST-Core-Nodes'
}

{ #category : 'visitor' }
RBQueryBlockNode >> acceptVisitor: aProgramNodeVisitor [
  ^ aProgramNodeVisitor acceptQueryBlockNode: self
]
