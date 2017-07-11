"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
details

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	self printDetailsOn: stream.
	^stream contents