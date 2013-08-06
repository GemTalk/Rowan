tests
test_unescapePercents

	self
		assert: 'aa%20aa%20%C3%A9%C3%A9%20aa%20aa'  unescapePercents asByteArray decodeFromUTF8 asString
			equals: 'aa aa éé aa aa';
		assert: 'aa%20aa%20%E9%E9%20aa%20aa' unescapePercents
			equals: 'aa aa éé aa aa' asUnicodeString