"
notice: This work is protected by copyright. All rights reserved.
category: printing
"
printOn: aStream

	| className |
	className := self class name.
	aStream
		nextPutAll: (className first isVowel ifTrue:[ 'an ' ] ifFalse:[ 'a ' ]);
		nextPutAll: className;
		nextPutAll: '('.
	self printDetailsOn: aStream.
	aStream nextPutAll: ')'.
