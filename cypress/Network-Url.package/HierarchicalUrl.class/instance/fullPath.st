"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: printing
"
fullPath
	| ans |
	ans := WriteStream on: String new.
	path do: [ :pathElem |
		ans nextPut: $/.
		ans nextPutAll: pathElem encodeForHTTP. ].
	self query isNil ifFalse: [ 
		ans nextPut: $?.
		ans nextPutAll: self query. ].
	self fragment isNil ifFalse: [
		ans nextPut: $#.
		ans nextPutAll: self fragment encodeForHTTP. ].
	
	^ans contents