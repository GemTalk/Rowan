matching
matchString: aString

	^ patternMatchBlock cull: pattern cull: aString
