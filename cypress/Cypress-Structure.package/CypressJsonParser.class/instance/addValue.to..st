"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: adding
"
addValue: anObject to: aCollection
	"Add anObject to aCollection. Subclasses might want to refine this implementation."

	^ aCollection copyWith: anObject
