"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
competingPackageNamesString

	^String streamContents: 
			[:stream |
			self competingPackageNames
				do: [:each | stream nextPutAll: each printString]
				separatedBy: [stream nextPutAll: ', ']]