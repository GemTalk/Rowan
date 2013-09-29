"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: parsing
"
parse

	| result |
	result := self
				whitespace;
				parseValue.
	stream atEnd ifFalse: [CypressJsonError signal: 'end of input expected'].
	^result