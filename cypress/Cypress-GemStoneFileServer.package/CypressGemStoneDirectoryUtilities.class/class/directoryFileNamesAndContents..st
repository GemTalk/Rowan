"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: unknown
"
directoryFileNamesAndContents: aDirectory
	"Walk the directory tree starting at aDirectory and
	 answer a map of the names of the files in the tree to
	 their contents (which work best when text)."

	| map |
	map := Dictionary new.
	self directoryFileNamesAndContents: aDirectory into: map.
	^map.
