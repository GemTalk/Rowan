accessing
packageFormat: aString

	(#( 'tonel' 'filetree' ) includes: aString)
		ifFalse: [ self error: 'Unknown package format ', aString printString, '. Should be one of: tonel, or filetree' ].
	^ self properties at: 'packageFormat' put: aString