"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: parsing-internal
"
parseCharacterHex
	| value |
	value := self parseCharacterHexDigit.
	3 timesRepeat: [ value := (value << 4) + self parseCharacterHexDigit ].
	^ Character codePoint: value
