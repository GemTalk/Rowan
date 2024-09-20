Extension { #name : 'Array' }

{ #category : '*Cypress-GemStone-PackageManagement' }
Array >> _writeCypressJsonOn: aStream indent: startIndent [
	"Private method which may be removed in a future GemStone version."

	| indent |
	aStream nextPutAll: '['.
	indent := startIndent + 1.
	1 to: self size
		do: 
			[:index |
			| item |
			item := self at: index.
			aStream lf.
			indent timesRepeat: [ aStream tab ].
			item _writeCypressJsonOn: aStream indent: indent.
			index < self size
				ifTrue: 
					[ aStream nextPutAll: ',']  ].
	self size > 0
		ifTrue: [  
			aStream lf.
			 startIndent timesRepeat: [ aStream tab ].
			 aStream nextPutAll: ']' ]
		ifFalse: [ aStream nextPutAll: ' ]' ]
]
