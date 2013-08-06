"
notice: This work is protected by copyright. All rights reserved.
category: parsing
"
parse

	| result |
	result := self
				whitespace;
				parseValue.
	stream atEnd ifFalse: [CypressJsonError signal: 'end of input expected'].
	^result