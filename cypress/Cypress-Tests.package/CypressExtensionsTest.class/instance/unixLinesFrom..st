"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: utility
"
unixLinesFrom: aString
	
	| sourceStream resultStream |
	sourceStream := ReadStreamPortable on: aString withUnixLineEndings.
	resultStream := WriteStreamPortable on: Array new.
	[sourceStream atEnd]
		whileFalse: [resultStream nextPut: (sourceStream upTo: Character lf)].
	^resultStream contents.
