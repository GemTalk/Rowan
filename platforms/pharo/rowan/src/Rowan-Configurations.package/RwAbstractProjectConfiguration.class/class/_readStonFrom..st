private
_readStonFrom: stream

	| reader | 
	(reader := STONReader on: stream)
		allowComplexMapKeys: true.
	^reader next
		initializeForImport;
		yourself
