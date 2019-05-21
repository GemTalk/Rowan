Class {
	#name : 'RBQueryBlockNode',
	#superclass : 'RBBlockNode',
	#category : 'AST-Core'
}

{ #category : 'visitor' }
RBQueryBlockNode >> acceptVisitor: aProgramNodeVisitor [
  ^ aProgramNodeVisitor acceptQueryBlockNode: self
]
