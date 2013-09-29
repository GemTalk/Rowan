"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: utilities
"
directoryEntriesFrom: aDirectory
	"Answer fully qualified paths to the contents of aDirectory."

	^(GsFile contentsOfDirectory: aDirectory onClient: false) ifNil: [#()]