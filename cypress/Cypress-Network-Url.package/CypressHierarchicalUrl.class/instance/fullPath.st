"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: printing
"
fullPath

	| ans |
	ans := WriteStreamPortable on: String new.
	path do: 
			[:pathElem |
			ans nextPut: $/.
			self writeWithHttpEscapes: pathElem on: ans].
	self query isNil
		ifFalse: 
			[ans nextPut: $?.
			ans nextPutAll: self query].
	self fragment isNil
		ifFalse: 
			[ans nextPut: $#.
			self writeWithHttpEscapes: self fragment on: ans].
	^ans contents
