"
notice: This work is protected by copyright. All rights reserved.
category: utilities
"
directoryEntriesFrom: aDirectory
	"Answer fully qualified paths to the contents of aDirectory."

	^(GsFile contentsOfDirectory: aDirectory onClient: false) ifNil: [#()]