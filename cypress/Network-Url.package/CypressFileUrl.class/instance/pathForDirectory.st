"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: paths
"
pathForDirectory
	"Path using local file system's pathname delimiter.
	DOS paths with drive letters should not
	be prepended with a delimiter even though
	they are absolute. Filename is left out."

	| delimiter s |
	delimiter := CypressFileUtilities current pathNameDelimiter.
	s := WriteStreamPortable on: (String new: 100).
	(self isAbsolute and: [self firstPartIsDriveLetter not])
		ifTrue: [s nextPutAll: delimiter].
	1 to: self path size - 1
		do: 
			[:ii |
			s
				nextPutAll: (path at: ii);
				nextPutAll: delimiter].
	^s contents