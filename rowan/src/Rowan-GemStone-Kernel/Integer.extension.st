Extension { #name : 'Integer' }

{ #category : '*rowan-gemstone-kernel' }
Integer >> rwSemanticIntegerLessThanSelf: anInteger [

	^ anInteger < self

]

{ #category : '*rowan-gemstone-kernel' }
Integer >> rwSemanticStringLessThanSelf:  aString [
  "integers have greater precedence than strings"
	" aString < anInteger -> false"

  ^ false

]

{ #category : '*rowan-gemstone-kernel' }
Integer >> rwSemanticVersionComponentLessThan: aRwSemanticVersonComponent [

	^ aRwSemanticVersonComponent rwSemanticIntegerLessThanSelf: self

]
