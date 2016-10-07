"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: parsing-internal
"
parseCharacterHex
  | value |
  value := self parseCharacterHexDigit.
  3 timesRepeat: [ value := (value bitShift: 4) + self parseCharacterHexDigit ].
  ^ Character codePoint: value