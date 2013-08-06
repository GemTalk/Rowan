"
notice: This work is protected by copyright. All rights reserved.
category: parsing
"
parseObject

	| result |
	self expect: '{'.
	result := self createObject.
	(self match: '}') ifTrue: [^result].
	[stream atEnd] whileFalse: 
			[result := self addProperty: self parseProperty to: result.
			(self match: '}') ifTrue: [^result].
			self expect: ','].
	CypressJsonError signal: 'end of object expected'