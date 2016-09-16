"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-GemStoneFileServer-Adding
"
nextChunkPut: aString

	aString do: 
		[:each |
		self nextPut: each.
		each = $! ifTrue: [self nextPut: each]].
	self nextPut: $!.
