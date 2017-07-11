"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
competingPackageNamesString

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	self competingPackageNames
		do: [:each | stream nextPutAll: each printString]
		separatedBy: [stream nextPutAll: ', '].
	^stream contents
