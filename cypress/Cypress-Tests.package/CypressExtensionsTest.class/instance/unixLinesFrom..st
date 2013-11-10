"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: utility
"
unixLinesFrom: aString
	
	| sourceStream resultStream |
	sourceStream := ReadStream on: aString withUnixLineEndings.
	resultStream := WriteStream on: Array new.
	[sourceStream atEnd]
		whileFalse: [resultStream nextPut: (sourceStream upTo: Character lf)].
	^resultStream contents.
