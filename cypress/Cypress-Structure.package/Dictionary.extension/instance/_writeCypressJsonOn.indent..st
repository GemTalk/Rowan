"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-Structure
"
_writeCypressJsonOn: aStream indent: startIndent
	"Private method which may be removed in a future GemStone version."

	| indent cnt |
	indent := startIndent.
	aStream
		nextPutAll: '{';
		lf.
	cnt := 0.
	indent := indent + 1.
	self keys asSortedCollection do: 
			[:key |
			| value |
			value := self at: key.
			cnt := cnt + 1.
			indent timesRepeat: [aStream tab].
			key _writeCypressJsonOn: aStream indent: indent.
			aStream nextPutAll: ' : '.
			value _writeCypressJsonOn: aStream indent: indent.
			cnt < self size
				ifTrue: 
					[aStream
						nextPutAll: ',';
						lf]].
	self size = 0 ifTrue: [indent timesRepeat: [aStream tab]].
	aStream nextPutAll: ' }'