"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: paths
"
pathDirString
	"Path to directory as url, using slash as delimiter.
	Filename is left out."

	| s |
	s := WriteStreamPortable on: (String new: 100).
	isAbsolute ifTrue: [s nextPut: $/].
	1 to: self path size - 1
		do: 
			[:ii |
			s
				nextPutAll: (path at: ii);
				nextPut: $/].
	^s contents