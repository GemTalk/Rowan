*Cypress-Structure
writeCypressJsonOn: aStream indent: startIndent

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
			aStream tab: indent.
			key writeCypressJsonOn: aStream indent: indent.
			aStream nextPutAll: ' : '.
			value writeCypressJsonOn: aStream indent: indent.
			cnt < self size
				ifTrue: 
					[aStream
						nextPutAll: ',';
						lf]].
	self size = 0 ifTrue: [aStream tab: indent].
	aStream nextPutAll: ' }'