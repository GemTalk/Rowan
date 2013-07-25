parsing
parse

	| result |
	result := self
				whitespace;
				parseValue.
	stream atEnd ifFalse: [CypressJsonError signal: 'end of input expected'].
	^result