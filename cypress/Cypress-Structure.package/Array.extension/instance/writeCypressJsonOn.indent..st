"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-Structure
"
writeCypressJsonOn: aStream indent: startIndent

	| indent |
	aStream
		nextPutAll: '[';
		lf.
	indent := startIndent + 1.
	1 to: self size
		do: 
			[:index |
			| item |
			item := self at: index.
			indent timesRepeat: [aStream tab].
			item writeCypressJsonOn: aStream indent: indent.
			index < self size
				ifTrue: 
					[aStream
						nextPutAll: ',';
						lf]].
	self size = 0 ifTrue: [indent timesRepeat: [aStream tab]].
	aStream nextPutAll: ' ]'