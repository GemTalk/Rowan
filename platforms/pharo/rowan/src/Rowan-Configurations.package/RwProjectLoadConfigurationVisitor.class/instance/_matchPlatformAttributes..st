private
_matchPlatformAttributes: platformPatternMatcher

	self platformAttributes do: [:anObject |
		(platformPatternMatcher match: anObject) ifTrue: [ ^true ] ].
	^false
