"
notice: This work is protected by copyright. All rights reserved.
category: parsing-internal
"
parseProperty
	| name value |
	name := self parseString.
	self expect: ':'.
	value := self parseValue.
	^ self createProperty: name with: value.
