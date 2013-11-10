"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: parsing-internal
"
parseProperty
	| name value |
	name := self parseString.
	self expect: ':'.
	value := self parseValue.
	^ self createProperty: name with: value.
