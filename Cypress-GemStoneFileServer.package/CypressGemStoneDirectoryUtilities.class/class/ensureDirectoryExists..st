"
notice: This work is protected by copyright. All rights reserved.
category: utilities
"
ensureDirectoryExists: aDirectory

	| lastSeparator |
	(GsFile existsOnServer: aDirectory) ifTrue: [^aDirectory].
	(GsFile createServerDirectory: aDirectory) ifNotNil: [^aDirectory].
	lastSeparator := aDirectory findLastSubString: self pathNameDelimiter startingAt: aDirectory size.
	lastSeparator <= 1 ifTrue: [self error: 'Cannot create directory'].
	self ensureDirectoryExists: (aDirectory copyFrom: 1 to: lastSeparator - 1).
	self ensureDirectoryExists: aDirectory.
