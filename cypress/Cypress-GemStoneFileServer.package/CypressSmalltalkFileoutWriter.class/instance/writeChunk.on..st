"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
writeChunk: aString on: aStream

	aString do: 
			[:each |
			aStream nextPut: each.
			each = $! ifTrue: [aStream nextPut: each]].
	aStream nextPut: $!