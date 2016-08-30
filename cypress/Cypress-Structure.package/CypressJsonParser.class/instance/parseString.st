"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: parsing-internal
"
parseString
	| result |
	self expect: '"'.
	result := WriteStreamPortable on: String new.
	[ stream atEnd or: [ stream peek = $" ] ] 
		whileFalse: [ result nextPut: self parseCharacter ].
	^ self expect: '"'; createString: result contents
