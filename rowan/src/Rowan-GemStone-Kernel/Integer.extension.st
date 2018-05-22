Extension { #name : 'Integer' }

{ #category : '*rowan-gemstone-kernel' }
Integer >> rwSemanticIntegerLessThanSelf: anInteger [

	^ anInteger < self

]

{ #category : '*rowan-gemstone-kernel' }
Integer >> rwSemanticStringLessThanSelf:  aString [
  "string version components are always '<' integer component"

  ^ true

]

{ #category : '*rowan-gemstone-kernel' }
Integer >> rwSemanticVersionComponentLessThan: aRwSemanticVersonComponent [

	^ aRwSemanticVersonComponent rwSemanticIntegerLessThanSelf: self

]
