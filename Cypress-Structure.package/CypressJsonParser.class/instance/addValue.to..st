"
notice: This work is protected by copyright. All rights reserved.
category: adding
"
addValue: anObject to: aCollection
	"Add anObject to aCollection. Subclasses might want to refine this implementation."

	^ aCollection copyWith: anObject
