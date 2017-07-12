"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
endsWithSpecial: filename
	"Answer true if the given filename ends with any of the special sequences
	'/..' '/.' '\..' '\.', false otherwise."

	| filenameSize finalChars |
	filenameSize := filename size.
	finalChars := filename copyFrom: filenameSize - 1 to: filenameSize.
	finalChars = '/.' ifTrue: [^true].
	finalChars = '\.' ifTrue: [^true].
	finalChars := filename copyFrom: filenameSize - 2 to: filenameSize.
	finalChars = '/..' ifTrue: [^true].
	finalChars = '\..' ifTrue: [^true].
	^false