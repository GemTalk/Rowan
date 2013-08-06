"
notice: This work is protected by copyright. All rights reserved.
category: parsing-internal
"
parseCharacterHex
	| value |
	value := self parseCharacterHexDigit.
	3 timesRepeat: [ value := (value << 4) + self parseCharacterHexDigit ].
	^ Character codePoint: value
