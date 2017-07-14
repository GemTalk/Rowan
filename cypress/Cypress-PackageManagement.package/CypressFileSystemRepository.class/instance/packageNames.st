"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
packageNames

	| extension extensionSize |
	extension := self packageExtension.
	extensionSize := extension size.
	^(self fileUtils
		directoryEntriesFrom: self directoryPath , '*' , extension) collect: 
				[:each |
				| localName |
				localName := self fileUtils localNameFrom: each.
				localName copyFrom: 1 to: localName size - extensionSize]
