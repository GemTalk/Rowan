comparing - private
currentAdditions

	currentAdditions ifNil: [self updateCurrentAdditionsAndRemovals].
	^currentAdditions