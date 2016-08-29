"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
test_streamContents_

	self
		assert: (String streamContents: 
					[:stream |
					(1 to: 3) do: [:each | stream nextPutAll: each printString]
						separatedBy: [stream space]])
		equals: '1 2 3'