"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: printing
"
printOn: aStream

	aStream nextPutAll: self schemeName.
	aStream nextPutAll: '://'.
	self username
		ifNotNil: 
			[self writeWithHttpEscapes: self username on: aStream.
			self password
				ifNotNil: 
					[aStream nextPutAll: ':'.
					self writeWithHttpEscapes: self password on: aStream].
			aStream nextPutAll: '@'].
	aStream nextPutAll: self authority.
	port
		ifNotNil: 
			[aStream
				nextPut: $:;
				nextPutAll: port printString].
	path do: 
			[:pathElem |
			aStream nextPut: $/.
			self writeWithHttpEscapes: pathElem on: aStream].
	self query isNil
		ifFalse: 
			[aStream nextPut: $?.
			aStream nextPutAll: self query].
	self fragment isNil
		ifFalse: 
			[aStream nextPut: $#.
			self writeWithHttpEscapes: self fragment on: aStream]