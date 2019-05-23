private
_toSTON: anObject
	^ (String streamContents: [ :stream | 
		(TonelSTONWriter on: stream) nextPut: anObject ])
		withLineEndings: self _newLine