Extension { #name : 'Integer' }

{ #category : '*rowan-gemstone-components-kernel' }
Integer >> rwSemanticIntegerLessThanSelf: anInteger [

	^ anInteger < self

]

{ #category : '*rowan-gemstone-components-kernel' }
Integer >> rwSemanticStringLessThanSelf:  aString [
  "integers have greater precedence than strings"
	" aString < anInteger -> false"

  ^ false

]

{ #category : '*rowan-gemstone-components-kernel' }
Integer >> rwSemanticVersionComponentLessThan: aRwSemanticVersonComponent [

	^ aRwSemanticVersonComponent rwSemanticIntegerLessThanSelf: self

]
